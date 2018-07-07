module Main where

import Control.Monad

import StatusNotifier.Item.NotificationOverlay

params = NotificationOverlayItemParams
  { iconName = "github"
  , itemDBusName = "org.Github.Notifications"
  , getOverlayName = undefined
  }

main :: IO ()
main = do
  buildNotificationOverlayItem params
  void getChar
