module Main where

import Control.Monad

import StatusNotifier.Item.Notifications.OverlayIcon

params = OverlayIconParams
  { iconName = "github"
  , iconPath = "/StatusNotifierItem"
  , iconDBusName = "org.Github.Notifications"
  , getOverlayName = const $ return "steam"
  , listenForNotifications = sampleNotificationListener
  }

main :: IO ()
main = do
  buildOverlayIcon params
  void getChar
