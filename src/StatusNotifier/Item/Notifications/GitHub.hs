module StatusNotifier.Item.Notifications.GitHub where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
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

githubNotificationsNew config@GitHubConfig
                         { ghAuth = auth
                         , ghRefreshSeconds = refreshSeconds
                         } update = do

  let getNotificationsFromGitHub = getNotifications auth
      logAndShow :: (Show v) => Priority -> String -> v -> IO ()
      logAndShow level message value =
        ghLog level $ printf message (show value)

  notificationsVar <- MV.newMVar V.empty
  -- TODO: indicate error somehow
  errorVar <- MV.newMVar Nothing

  let openNotificationsHTML = openURL "https://github.com/notifications"
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
        onMenuitemItemActivated markAllReadItem $ const $ void markAllRead
        menuitemChildAppend root markAllReadItem

        viewItem <- makeMenuItemWithLabel "View notifications"
        onMenuitemItemActivated viewItem $ const $ void $ openNotificationsHTML
        menuitemChildAppend root viewItem

        return root
      updateNotifications newNotifications currentNotifications =
        let newSortedIds = sort $ map notificationId $ V.toList newNotifications
            oldSortedIds = sort $ map notificationId $ V.toList currentNotifications
        in return (newNotifications, newSortedIds /= oldSortedIds)
      updateError error =
        MV.modifyMVar_ errorVar (const return error) >> return False
      updateVariables =
        getNotificationsFromGitHub >>=
        either updateError (MV.modifyMVar notificationsVar . updateNotifications)

  void $ forkIO $ forever $ do
    menuNeedsRebuild <- updateVariables
    when menuNeedsRebuild $ do
      newRoot <- buildMenu
      notificationsCount <- V.length <$> getCurrentNotifications
      update notificationsCount newRoot
    threadDelay (floor $ refreshSeconds * 1000000)

makeNotificationItem :: GitHubConfig -> Notification -> IO Menuitem
makeNotificationItem GitHubConfig { ghAuth = auth }
                     notification@Notification
                       { notificationId = thisNotificationId
                       , notificationSubject = Subject
                         { subjectTitle = title }
                       , notificationRepo = RepoRef
                         { repoRefRepo = repositoryName }
                       } = do
  let notificationText = T.pack $ printf "%s - %s"
                         (untagName repositoryName) title
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
