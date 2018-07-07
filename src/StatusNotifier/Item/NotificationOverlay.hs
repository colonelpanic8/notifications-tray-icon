{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Item.NotificationOverlay where

import           Control.Concurrent
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
import           StatusNotifier.Item.GitHub
import qualified StatusNotifier.Watcher.Client as W

-- TODO rename overlay Icon

data NotificationOverlayItemParams = NotificationOverlayItemParams
  { iconName :: String
  , itemDBusName :: String
  , getOverlayName :: Int -> IO T.Text
  }

buildNotificationOverlayItem NotificationOverlayItemParams
                               { iconName = name
                               , itemDBusName = dbusName
                               , getOverlayName = getOverlayIconName
                               } = do
  client <- connectSession
  let menuPathString = "/StatusNotifierItem/Menu"
      menuBusString = "org.GitHub.Notifications.Menu2"
  let clientInterface =
        Interface { interfaceName = "org.kde.StatusNotifierItem"
                  , interfaceMethods = []
                  , interfaceProperties =
                    [ readOnlyProperty "IconName" $ return name
                    , readOnlyProperty "OverlayIconName" $ return ("steam" :: String)
                    , readOnlyProperty "Menu" $ return $ objectPath_ $ T.unpack menuPathString
                    ]
                  , interfaceSignals = []
                  }
  export client (fromString "/StatusNotifierItem") clientInterface
  requestName client (busName_ dbusName) []

  connection <- Gio.busGetSync Gio.BusTypeSession Gio.noCancellable
  Gio.busOwnNameOnConnection connection menuBusString [] Nothing Nothing
  menuServer <- serverNew menuPathString
  root <- makeMenuItemWithLabel "Testitem"
  child <- makeMenuItemWithLabel "child"
  menuitemChildAppend root child

  let setupServer = do
        serverSetRoot menuServer root
        return True

  loop <- GLib.mainLoopNew Nothing False >>= GLib.mainLoopRef
  forkIO $ GLib.mainLoopRun loop
  context <- GLib.mainLoopGetContext loop

  threadDelay 999999
  GLib.mainLoopIsRunning loop >>= print
  GLib.mainContextInvokeFull context 4 setupServer

  threadDelay 5000000

  proxyAll client (busName_ $ T.unpack menuBusString) (objectPath_ $ T.unpack menuPathString)
             (objectPath_ $ T.unpack menuPathString)

  W.registerStatusNotifierItem client dbusName
