module StatusNotifier.Item.Notifications.OverlayIcon where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           DBus
import           DBus.Client
import           DBus.Proxy
import qualified DBus.TH as DBusTH
import qualified Data.ByteString as BS
import           Data.Int
import           Data.String
import qualified Data.Text as T
import           GI.Dbusmenu
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified StatusNotifier.Item.Client as I
import           StatusNotifier.Item.Notifications.GitHub
import qualified StatusNotifier.Watcher.Client as W

type UpdateNotifications = Int -> Menuitem -> IO ()

data OverlayIconParams = OverlayIconParams
  { iconName :: String
  , iconPath :: String
  , iconDBusName :: String
  , getOverlayName :: Int -> IO T.Text
  , listenForNotifications :: UpdateNotifications -> IO ()
  }

buildOverlayIcon OverlayIconParams
                   { iconName = name
                   , iconPath = path
                   , iconDBusName = dbusName
                   , getOverlayName = getOverlayIconName
                   , listenForNotifications = startNotifications
                   } = do
  let menuPathString = path ++ "/Menu"
      menuBusString = dbusName ++ ".Menu"
      menuPathText = T.pack menuPathString
      menuBusText = T.pack menuBusString
      iconObjectPath = objectPath_ path

  client <- connectSession

  notificationCount <- newMVar 0
  root <- menuitemNew
  currentRoot <- newMVar root

  connection <- Gio.busGetSync Gio.BusTypeSession Gio.noCancellable
  Gio.busOwnNameOnConnection connection menuBusText [] Nothing Nothing
  menuServer <- serverNew menuPathText

  mainLoop <- GLib.mainLoopNew Nothing False >>= GLib.mainLoopRef
  forkIO $ GLib.mainLoopRun mainLoop
  context <- GLib.mainLoopGetContext mainLoop

  let runOnMain action = do
        GLib.mainContextInvokeFull context 4 $ action >> return False
      setRoot newRoot = runOnMain $ do
          putStrLn "Setting new root"
          modifyMVar_ currentRoot $ const $ return newRoot
          serverSetRoot menuServer newRoot
          return False
      updateOverlayCount count = do
        modifyMVar_ notificationCount $ const $ return count
        I.emitNewOverlayIcon client iconObjectPath
      updateNotifications count newRoot = void $
        updateOverlayCount count >> setRoot newRoot
      proxyMenu =
        proxyAll client
                   (busName_ menuBusString)
                   (objectPath_ menuPathString)
                   (objectPath_ menuPathString)
      clientInterface =
        Interface { interfaceName = "org.kde.StatusNotifierItem"
                  , interfaceMethods = []
                  , interfaceProperties =
                    [ readOnlyProperty "IconName" $ return name
                    , readOnlyProperty "OverlayIconName" $
                      (readMVar notificationCount) >>= getOverlayIconName
                    , readOnlyProperty "Menu" $ return $ objectPath_ menuPathString
                    ]
                  , interfaceSignals = []
                  }

  export client (fromString path) clientInterface
  requestName client (busName_ dbusName) []

  startNotifications updateNotifications
  proxyMenu

  W.registerStatusNotifierItem client dbusName
