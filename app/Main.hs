module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as BS
import           Data.Semigroup ((<>))
import           Data.Version (showVersion)
import qualified GitHub.Auth as GH
import           Options.Applicative
import           StatusNotifier.Item.Notifications.GitHub
import           StatusNotifier.Item.Notifications.OverlayIcon
import           System.Console.Haskeline
import           Text.Printf

import           Paths_notifications_tray_icon (version)

iconNameParser = strOption
  (  long "icon-name"
  <> short 'n'
  <> metavar "NAME"
  <> value "github"
  <> help "The icon the item will display"
  )

overlayIconNameParser = strOption
  (  long "overlay-icon-name"
  <> short 'o'
  <> metavar "NAME"
  <> value "github"
  <> help "The overlay icon that will be displayed when notifications are present"
  )

githubPassTokenParser :: Parser (IO GH.Auth)
githubPassTokenParser = githubAuthFromPass <$> strOption
  (  long "github-pass-token"
  <> metavar "TOKEN-NAME"
  <> help "Use pass to get a token password to authenticate with github"
  )

getUsernameAndPassword = runInputT defaultSettings $ do
  Just username <- getInputLine "username: "
  Just password <- getPassword (Just '*') "password: "
  return (username, password)

githubAuthFromUsernamePassword (username, password) =
  GH.BasicAuth (BS.pack username) (BS.pack password)

githubConsoleAuthParser :: Parser (IO GH.Auth)
githubConsoleAuthParser =
  flag' (githubAuthFromUsernamePassword <$> getUsernameAndPassword) $
  long "github-basic-auth"

githubAuthParser = githubConsoleAuthParser <|> githubPassTokenParser

githubParser :: Parser (IO GitHubConfig)
githubParser = fmap <$> helper <*> githubAuthParser
  where helper =
          flip GitHubConfig <$> option auto
            (  long "poll-interval"
            <> help "The amount of time to wait between refreshes of notification data"
            <> value 30
            <> metavar "SECONDS"
            )

updaterParser
  =   (fmap githubUpdaterNew <$> githubParser)
  <|> (flag' (return $ const $ return ()) $ long "no-updater")

params iconName overlayIconName notifications = OverlayIconParams
  { iconName = "github"
  , iconPath = "/StatusNotifierItem"
  , iconDBusName = "org.Github.Notifications"
  , getOverlayName = \count -> return $ if count > 0 then overlayIconName  else ""
  , runUpdater = notifications
  }

startOverlayIcon getUpdater iconName overlayIconName =
  (params iconName overlayIconName <$> getUpdater) >>= buildOverlayIcon

parser = startOverlayIcon <$> updaterParser <*> iconNameParser <*> overlayIconNameParser

versionOption :: Parser (a -> a)
versionOption = infoOption
                (printf "notifications-tray-icon %s" versionString)
                (  long "version"
                <> (help $
                    printf "Show the version number of notifications-tray-icon (%s)"
                    versionString)
                )
  where versionString = showVersion version

main :: IO ()
main = do
  join $ execParser $ info (helper <*> versionOption <*> parser)
         (  fullDesc
         <> progDesc "Run a standalone StatusNotifierItem/AppIndicator tray"
         )
  void $ forever $ getChar
