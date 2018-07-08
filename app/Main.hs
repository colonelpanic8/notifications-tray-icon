module Main where

import Control.Monad

import StatusNotifier.Item.Notifications.GitHub
import StatusNotifier.Item.Notifications.OverlayIcon

params notifications = OverlayIconParams
  { iconName = "github"
  , iconPath = "/StatusNotifierItem"
  , iconDBusName = "org.Github.Notifications"
  , getOverlayName = \count -> return $ if count > 0 then "steam" else ""
  , listenForNotifications = notifications
  }

main :: IO ()
main = do
  auth <- githubAuthFromPass "github-token"
  buildOverlayIcon $ params $ githubNotificationsNew $ defaultGitHubConfig auth
  void getChar
