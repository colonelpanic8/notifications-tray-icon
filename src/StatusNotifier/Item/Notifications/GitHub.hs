{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.Notifications.GitHub where

import           Control.Concurrent
import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import           Data.Int
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           GI.Dbusmenu
import qualified GitHub.Auth as Auth
import           GitHub.Data
import           GitHub.Endpoints.Activity.Notifications
import           Network.HTTP.Simple
import           System.Log.Logger
import           Text.Printf

ghLog :: Priority -> String -> IO ()
ghLog = logM "System.Taffybar.Widget.GitHubNotifications"

data GitHubConfig = GitHubConfig
  { ghAuth :: Auth.Auth
  , ghGetIconName :: Either Error (V.Vector Notification) -> T.Text
  , ghRefreshSeconds :: Rational
  }

errorOrNotifsLength :: (Show a1, Foldable t) => Either a1 (t a2) -> String
errorOrNotifsLength (Right notifs) = show $ length notifs
errorOrNotifsLength (Left e) = show e

defaultGithubConfig :: Auth -> GitHubConfig
defaultGithubConfig auth = GitHubConfig
  { ghAuth = auth
  , ghGetIconName = T.pack . errorOrNotifsLength
  , ghRefreshSeconds = 15
  }

githubNotificationsNew :: GitHubConfig -> IO ()
githubNotificationsNew config@GitHubConfig
                         { ghAuth = auth
                         , ghGetIconName = getIconNameFromNotifs
                         , ghRefreshSeconds = refreshSeconds
                         } = do
  menuClient <- clientNew "org.GitHub.Notifications.Menu" "/StatusNotifierItem/Menu"

  let getAllNotifications = getNotifications auth
      logAndShow :: (Show v) => Priority -> String -> v -> IO ()
      logAndShow level message value =
        ghLog level $ printf message (show value)

  notificationsVar <- getAllNotifications >>= MV.newMVar
  menuItemsVar <- MV.newMVar M.empty

  let getIconName = getIconNameFromNotifs <$> MV.readMVar notificationsVar
      updateNotifications = do
        newNotifications <- getAllNotifications
        either (logAndShow WARNING "Got error fetching github notifications %s")
                 ((logAndShow DEBUG "Got %s notifications from github") . V.length)
                 newNotifications
        void $ MV.swapMVar notificationsVar newNotifications

  menuRoot <- clientGetRoot menuClient

  let getNotificationsVector =
        (either (const V.empty) id <$> MV.readMVar notificationsVar)
      addToMenu = menuitemChildAppend menuRoot
      getNotificationItem c n@Notification { notificationId = thisNotificationId } =
        MV.modifyMVar menuItemsVar $ \menuItems -> do
          menuItem <- maybe (makeNotificationItem c n) return $
                      M.lookup thisNotificationId menuItems
          return (M.insert thisNotificationId menuItem menuItems, menuItem)
      populateMenu = do
        notifications <- getNotificationsVector
        mapM_ ((>>= addToMenu) . makeNotificationItem config) notifications
  return ()
        -- Gtk.separatorMenuItemNew >>= addToMenu

        -- viewAll <- Gtk.menuItemNewWithLabel (T.pack "View All")
        -- _ <- Gtk.onWidgetButtonPressEvent viewAll $ const $
        --      openURL "https://github.com/notifications" >>
        --      return True
        -- addToMenu viewAll

        -- markAllAsRead <- menuitemNew
        --                  (T.pack "Mark All As Read")
        -- addToMenu markAllAsRead

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
      -- openHTML = openNotificationHTML notification >> return True
  menuItem <- menuitemNewWithId $ fromIntegral $ untagId thisNotificationId
  textVariant <- liftIO $ toGVariant notificationText
  menuitemPropertySetVariant menuItem "label" textVariant

  markAsReadItem <- makeMenuItemWithLabel "Mark as read"
  viewItem <- makeMenuItemWithLabel "View on GitHub"

  menuitemChildAppend menuItem markAsReadItem
  menuitemChildAppend menuItem viewItem

  return menuItem

makeMenuItemWithLabel :: T.Text -> IO Menuitem
makeMenuItemWithLabel text = do
  menuItem <- menuitemNew
  textVariant <- liftIO $ toGVariant text
  menuitemPropertySetVariant menuItem "label" textVariant
  return menuItem

-- openNotificationHTML :: Notification -> IO ()
-- openNotificationHTML notification = do
--   let setUserAgent = setRequestHeader
--                    "User-Agent" ["Taffybar-GithubNotifier"]
--       request = setUserAgent $ parseRequest_ $ T.unpack $ getUrl $
--                 subjectURL $ notificationSubject notification
--   response <- httpLBS request
--   ghLog DEBUG $ printf "Got response from subject url: %s" $ show response
--   let maybeUrl = getHTMLURL $ getResponseBody response
--   void $ sequenceA $ openURL . T.unpack <$> maybeUrl
--   return ()

-- getHTMLURL :: LBS.ByteString -> Maybe T.Text
-- getHTMLURL jsonText = decode jsonText >>= parseMaybe (.: "html_url")
