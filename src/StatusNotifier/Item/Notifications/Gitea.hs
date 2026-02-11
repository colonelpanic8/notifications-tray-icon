module StatusNotifier.Item.Notifications.Gitea where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar as MV
import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Either
import           Data.List
import qualified Data.Text as T
import           GI.Dbusmenu
import           Gitea.API
import           StatusNotifier.Item.Notifications.Util
import           System.Log.Logger
import           Text.Printf

giteaLog :: Priority -> String -> IO ()
giteaLog = logM "StatusNotifier.Item.Notifications.Gitea"

data GiteaUpdaterConfig = GiteaUpdaterConfig
  { giteaConfig :: GiteaConfig
  , giteaRefreshSeconds :: Rational
  }

giteaUpdaterNew :: GiteaUpdaterConfig -> (Int -> Menuitem -> IO ()) -> IO ()
giteaUpdaterNew GiteaUpdaterConfig
                  { giteaConfig = config
                  , giteaRefreshSeconds = refreshSeconds
                  } update = do

  notificationsVar <- MV.newMVar []
  errorVar <- MV.newMVar Nothing
  forceRefreshVar <- MV.newEmptyMVar

  let forceRefresh = void $ MV.tryPutMVar forceRefreshVar ()
      delayedRefresh = void $ forkIO $ threadDelay 1000000 >> forceRefresh
      openNotificationsHTML =
        openURL $ giteaBaseUrl config <> "/notifications"
      markAllRead = void $ markAllNotificationsRead config
      getCurrentNotifications = MV.readMVar notificationsVar
      buildMenu = do
        notifications <- getCurrentNotifications
        root <- menuitemNew
        mapM_ ((>>= menuitemChildAppend root) . makeGiteaNotificationItem config)
            notifications

        separatorItem <- menuitemNew
        menuitemPropertySet separatorItem MENUITEM_PROP_TYPE CLIENT_TYPES_SEPARATOR
        menuitemChildAppend root separatorItem

        markAllReadItem <- makeMenuItemWithLabel "Mark all as read"
        onMenuitemItemActivated markAllReadItem $ const $ markAllRead >> delayedRefresh
        menuitemChildAppend root markAllReadItem

        viewItem <- makeMenuItemWithLabel "View notifications"
        onMenuitemItemActivated viewItem $ const $ void openNotificationsHTML
        menuitemChildAppend root viewItem

        refreshItem <- makeMenuItemWithLabel "Refresh"
        onMenuitemItemActivated refreshItem $ const forceRefresh
        menuitemChildAppend root refreshItem

        return root
      updateNotifications newNotifications currentNotifications =
        let newSortedIds = sort $ map giteaNotificationId newNotifications
            oldSortedIds = sort $ map giteaNotificationId currentNotifications
        in return ( newNotifications
                  , (newSortedIds /= oldSortedIds
                    , newSortedIds \\ oldSortedIds
                    )
                  )
      updateError err = do
        MV.modifyMVar_ errorVar (const $ return $ Just err)
        giteaLog ERROR $ printf "Error retrieving notifications %s" err
        return (False, [])
      updateVariables =
        getNotifications config >>=
        either updateError (MV.modifyMVar notificationsVar . updateNotifications)
      doUpdate = do
        newRoot <- buildMenu
        notificationsCount <- length <$> getCurrentNotifications
        update notificationsCount newRoot
      sendNotifications newIds = do
        notifications <- getCurrentNotifications
        let getById nid = find ((== nid) . giteaNotificationId) notifications
        mapM_ (traverse sendNotification . getById) newIds
      sendNotification notification =
        runCommandFromPath [ "notify-send"
                           , "--icon=gitea"
                           , getGiteaNotificationSummary notification
                           ]

  void updateVariables
  doUpdate
  void $ forkIO $ forever $ do
    result <- try $ do
      forced <-
        isRight <$> race (threadDelay (floor $ refreshSeconds * 1000000))
                         (takeMVar forceRefreshVar)
      giteaLog DEBUG "Refreshing notifications"
      (menuNeedsRebuild, newIds) <- updateVariables
      sendNotifications newIds
      giteaLog DEBUG $ printf "Rebuild needed: %s, force: %s"
                           (show menuNeedsRebuild) (show forced)
      when (forced || menuNeedsRebuild) doUpdate
    case (result :: Either SomeException ()) of
      Right () -> return ()
      Left err -> giteaLog ERROR $ printf "Exception in polling loop: %s" (show err)

makeGiteaNotificationItem :: GiteaConfig -> GiteaNotification -> IO Menuitem
makeGiteaNotificationItem config notification = do
  let notifId = giteaNotificationId notification
      notificationText = T.pack $ getGiteaNotificationSummary notification
      subject = giteaNotificationSubject notification
      openHTML = case giteaSubjectHtmlUrl subject of
        Just url -> void $ openURL $ T.unpack url
        Nothing -> case giteaSubjectLatestCommentHtmlUrl subject of
          Just url -> void $ openURL $ T.unpack url
          Nothing -> giteaLog WARNING "Notification has no HTML URL"
      markAsRead = void $ markNotificationRead config notifId

  menuItem <- menuitemNewWithId $ fromIntegral notifId
  textVariant <- toGVariant notificationText
  menuitemPropertySetVariant menuItem "label" textVariant

  markAsReadItem <- makeMenuItemWithLabel "Mark as read"
  onMenuitemItemActivated markAsReadItem $ const markAsRead
  menuitemChildAppend menuItem markAsReadItem

  viewItem <- makeMenuItemWithLabel "View on Gitea"
  onMenuitemItemActivated viewItem $ const openHTML
  menuitemChildAppend menuItem viewItem

  return menuItem

getGiteaNotificationSummary :: GiteaNotification -> String
getGiteaNotificationSummary notification =
  printf "%s - %s"
    (T.unpack $ giteaRepoFullName $ giteaNotificationRepo notification)
    (T.unpack $ giteaSubjectTitle $ giteaNotificationSubject notification)
