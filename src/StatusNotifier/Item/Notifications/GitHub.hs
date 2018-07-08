module StatusNotifier.Item.Notifications.GitHub where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either
import           Data.Int
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           GI.Dbusmenu
import qualified GitHub.Auth as Auth
import           GitHub.Data
import           GitHub.Endpoints.Activity.Notifications
import           Network.HTTP.Simple
import           StatusNotifier.Item.Notifications.Util
import           System.Log.Logger
import           Text.Printf

githubAuthFromPass passName = do
  Right (token, _) <- passGet passName
  return $ Auth.OAuth $ BS.pack token

ghLog :: Priority -> String -> IO ()
ghLog = logM "StatusNotifier.Item.Notifications.GitHub"

data GitHubConfig = GitHubConfig
  { ghAuth :: Auth.Auth
  , ghRefreshSeconds :: Rational
  }

defaultGitHubConfig :: Auth -> GitHubConfig
defaultGitHubConfig auth = GitHubConfig
  { ghAuth = auth
  , ghRefreshSeconds = 20
  }

githubUpdaterNew config@GitHubConfig
                   { ghAuth = auth
                   , ghRefreshSeconds = refreshSeconds
                   } update = do

  let getNotificationsFromGitHub = getNotifications auth
      logAndShow :: (Show v) => Priority -> String -> v -> IO ()
      logAndShow level message value =
        ghLog level $ printf message (show value)

  notificationsVar <- MV.newMVar V.empty
  errorVar <- MV.newMVar Nothing
  forceRefreshVar <- MV.newEmptyMVar

  let forceRefresh = void $ MV.tryPutMVar forceRefreshVar ()
      delayedRefresh = void $ forkIO $ threadDelay 1000000 >> forceRefresh
      openNotificationsHTML = openURL "https://github.com/notifications"
      markAllRead = markNotificationsAsRead auth
      getCurrentNotifications = MV.readMVar notificationsVar
      buildMenu = do
        notifications <- getCurrentNotifications
        root <- menuitemNew
        mapM_ ((>>= menuitemChildAppend root) . makeNotificationItem config)
            notifications

        separatorItem <- menuitemNew
        menuitemPropertySet separatorItem MENUITEM_PROP_TYPE CLIENT_TYPES_SEPARATOR
        menuitemChildAppend root separatorItem

        markAllReadItem <- makeMenuItemWithLabel "Mark all as read"
        onMenuitemItemActivated markAllReadItem $ const $ markAllRead >> delayedRefresh
        menuitemChildAppend root markAllReadItem

        viewItem <- makeMenuItemWithLabel "View notifications"
        onMenuitemItemActivated viewItem $ const $ void $ openNotificationsHTML
        menuitemChildAppend root viewItem

        refreshItem <- makeMenuItemWithLabel "Refresh"
        onMenuitemItemActivated refreshItem $ const $ forceRefresh
        menuitemChildAppend root refreshItem

        return root
      updateNotifications newNotifications currentNotifications =
        let newSortedIds = sort $ map notificationId $ V.toList newNotifications
            oldSortedIds = sort $ map notificationId $ V.toList currentNotifications
        in return ( newNotifications
                  , (newSortedIds /= oldSortedIds
                    , newSortedIds \\ oldSortedIds
                    )
                  )
      updateError error = do
        MV.modifyMVar_ errorVar (const return error)
        ghLog ERROR $ printf "Error retrieving notifications %s" $ show error
        return (False, [])
      updateVariables =
        getNotificationsFromGitHub >>=
        either updateError (MV.modifyMVar notificationsVar . updateNotifications)
      doUpdate = do
        newRoot <- buildMenu
        notificationsCount <- V.length <$> getCurrentNotifications
        update notificationsCount newRoot
      sendNotifications newIds = do
        notifications <- getCurrentNotifications
        let getById id = V.find ((== id) . notificationId) notifications
        mapM_ (traverse sendNotification . getById) newIds
      sendNotification notification =
        runCommandFromPath [ "notify-send"
                           , "--icon=github"
                           , getNotificationSummary notification
                           ]

  void $ updateVariables
  doUpdate
  void $ forkIO $ forever $ do
    forced <-
      isRight <$> race (threadDelay (floor $ refreshSeconds * 1000000))
                       (takeMVar forceRefreshVar)
    ghLog DEBUG "Refreshing notifications"
    (menuNeedsRebuild, newIds) <- updateVariables
    sendNotifications newIds
    ghLog DEBUG $ printf "Rebuild needed: %s, force: %s"
                         (show menuNeedsRebuild) (show forced)
    when (forced || menuNeedsRebuild) doUpdate

makeNotificationItem :: GitHubConfig -> Notification -> IO Menuitem
makeNotificationItem GitHubConfig { ghAuth = auth }
                     notification@Notification
                       { notificationId = thisNotificationId


                       } = do
  let notificationText = T.pack $ getNotificationSummary notification
      openHTML = openNotificationHTML notification
      markAsRead = markNotificationAsRead auth thisNotificationId

  menuItem <- menuitemNewWithId $ fromIntegral $ untagId thisNotificationId
  textVariant <- liftIO $ toGVariant notificationText
  menuitemPropertySetVariant menuItem "label" textVariant

  markAsReadItem <- makeMenuItemWithLabel "Mark as read"
  onMenuitemItemActivated markAsReadItem $ const $ void $ markAsRead
  menuitemChildAppend menuItem markAsReadItem

  viewItem <- makeMenuItemWithLabel "View on GitHub"
  onMenuitemItemActivated viewItem $ const openHTML
  menuitemChildAppend menuItem viewItem

  return menuItem

getNotificationSummary :: Notification -> String
getNotificationSummary
  Notification
  { notificationRepo =
      RepoRef
      { repoRefRepo = repositoryName }
  , notificationSubject =
    Subject
    { subjectTitle = title }
  } = printf "%s - %s" (untagName repositoryName) title

openNotificationHTML :: Notification -> IO ()
openNotificationHTML notification = do
  let setUserAgent = setRequestHeader
                   "User-Agent" ["Taffybar-GithubNotifier"]
      request = setUserAgent $ parseRequest_ $ T.unpack $ getUrl $
                subjectURL $ notificationSubject notification
  response <- httpLBS request
  ghLog DEBUG $ printf "Got response from subject url: %s" $ show response
  let maybeUrl = getHTMLURL $ getResponseBody response
  void $ sequenceA $ openURL . T.unpack <$> maybeUrl
  return ()

getHTMLURL :: LBS.ByteString -> Maybe T.Text
getHTMLURL jsonText = decode jsonText >>= parseMaybe (.: "html_url")
