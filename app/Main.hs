module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as BS
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Tuple.Sequence
import           Data.Version (showVersion)
import qualified GitHub.Auth as GH
import           Options.Applicative
import           StatusNotifier.Item.Notifications.GitHub
import           StatusNotifier.Item.Notifications.OverlayIcon
import           StatusNotifier.Item.Notifications.Util
import           System.Console.Haskeline
import           System.Log.Logger
import           Text.Printf

import           Paths_notifications_tray_icon (version)

iconNameParser :: Parser String
iconNameParser = strOption
  (  long "icon-name"
  <> short 'n'
  <> metavar "NAME"
  <> value "github"
  <> help "The icon the item will display"
  )

overlayIconNameParser :: Parser String
overlayIconNameParser = strOption
  (  long "overlay-icon-name"
  <> short 'o'
  <> metavar "NAME"
  <> value "github"
  <> help "The overlay icon that will be displayed when notifications are present"
  )

busNameParser :: Parser String
busNameParser = strOption
  (  long "bus-name"
  <> short 'b'
  <> metavar "BUS-NAME"
  <> value "org.Github.Notifications"
  )

githubTokenAuthParser :: Parser (IO GH.Auth)
githubTokenAuthParser = fmap (GH.OAuth . BS.pack . T.unpack . T.strip . T.pack) <$>
  (passGetMain <$> strOption
  (  long "github-token-pass"
  <> metavar "TOKEN-NAME"
  <> help "Use pass to get a token password to authenticate with github"
  ) <|>
  (gitConfigGet <$> strOption
  (  long "github-token-config"
  <> metavar "TOKEN-KEY"
  <> help "Get a github token using the provided git config key"
  )) <|>
  (return <$> strOption
  (  long "github-token-string"
  <> metavar "TOKEN"
  <> help "Provide the github token as a value"
  )))

gitConfigGet :: String -> IO String
gitConfigGet key = do
  Right value <- runCommandFromPath ["git", "config", "--get", key]
  return value

githubConfigAuthParser :: Parser (IO GH.Auth)
githubConfigAuthParser =
  fmap githubAuthFromUsernamePassword <$> usernamePasswordParser
  where usernamePasswordParser =
          runGitConfigCommands <$> userOption <*> passwordOption
        userOption = strOption
                 (  long "github-config-user"
                 <> metavar "USER-KEY"
                 <> help "The git config key to use to get the github user"
                 )
        passwordOption = strOption
                 (  long "github-config-password"
                 <> metavar "PASSWORD-KEY"
                 <> help "The git config key to use to get the github password"
                 )
        runGitConfigCommands userKey passwordKey = do
          username <- gitConfigGet userKey
          password <- gitConfigGet passwordKey
          return (username, password)

getUsernameAndPassword :: IO (String, String)
getUsernameAndPassword =
  runInputT defaultSettings $ sequenceT (getU, getP)
    where getP :: InputT IO String
          getP = getPassword (Just '*') "password: " >>= maybe getP pure
          getU :: InputT IO String
          getU = getInputLine "username: " >>= maybe getU pure

githubAuthFromUsernamePassword (username, password) =
  GH.BasicAuth (BS.pack username) (BS.pack password)

githubConsoleAuthParser :: Parser (IO GH.Auth)
githubConsoleAuthParser =
  flag' (githubAuthFromUsernamePassword <$> getUsernameAndPassword) $
  long "github-basic-auth"

githubAuthParser =
  githubTokenAuthParser <|> githubConfigAuthParser <|> githubConsoleAuthParser

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
  <|> (flag' (return $ sampleUpdater ) $ long "sample")

logParser =
  option auto
  (  long "log-level"
  <> short 'l'
  <> help "Set the log level"
  <> metavar "LEVEL"
  <> value WARNING
  )

params iconName overlayIconName busName notifications = OverlayIconParams
  { iconName = iconName
  , iconPath = "/StatusNotifierItem"
  , iconDBusName = busName
  , getOverlayName = \count -> return $ if count > 0 then T.pack overlayIconName else ""
  , runUpdater = notifications
  }

startOverlayIcon getUpdater iconName overlayIconName logLevel busName = do
  logger <- getLogger "StatusNotifier.Item.Notifications"
  saveGlobalLogger $ setLevel logLevel logger
  dbusLogger <- getLogger "DBus"
  saveGlobalLogger $ setLevel logLevel dbusLogger
  (params iconName overlayIconName busName <$> getUpdater) >>= buildOverlayIcon

parser =
  startOverlayIcon
  <$> updaterParser <*> iconNameParser <*> overlayIconNameParser
  <*> logParser <*> busNameParser

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
         <> progDesc "Run a notification monitoring tray icon"
         )
  void $ forever $ threadDelay 999999999999999999
