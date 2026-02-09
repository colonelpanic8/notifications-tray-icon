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
import           Gitea.API
import           Options.Applicative
import           StatusNotifier.Item.Notifications.Gitea
import           StatusNotifier.Item.Notifications.GitHub
import           StatusNotifier.Item.Notifications.Gmail
import           StatusNotifier.Item.Notifications.OverlayIcon
import           StatusNotifier.Item.Notifications.Util
import           System.Console.Haskeline
import           System.Log.Logger
import           Text.Printf

import           Paths_notifications_tray_icon (version, getDataDir)
import           System.FilePath ((</>))

-- | Shared options that apply to all subcommands.
data SharedOpts = SharedOpts
  { sharedIconName    :: Maybe String
  , sharedOverlay     :: Maybe String
  , sharedBusName     :: Maybe String
  , sharedLogLevel    :: Priority
  }

sharedOptsParser :: Parser SharedOpts
sharedOptsParser = SharedOpts
  <$> optional (strOption
        (  long "icon-name"
        <> short 'n'
        <> metavar "NAME"
        <> help "The icon the item will display"
        ))
  <*> optional (strOption
        (  long "overlay-icon-name"
        <> short 'o'
        <> metavar "NAME"
        <> help "The overlay icon that will be displayed when notifications are present"
        ))
  <*> optional (strOption
        (  long "bus-name"
        <> short 'b'
        <> metavar "BUS-NAME"
        ))
  <*> option auto
        (  long "log-level"
        <> short 'l'
        <> help "Set the log level"
        <> metavar "LEVEL"
        <> value WARNING
        )

-- | Per-subcommand defaults.
data SubcommandDefaults = SubcommandDefaults
  { defaultIconName :: String
  , defaultBusName  :: String
  }

-- | Resolve shared options with per-subcommand defaults.
resolveOpts :: SharedOpts -> SubcommandDefaults -> (String, String, String, Priority)
resolveOpts shared defaults =
  ( maybe (defaultIconName defaults) id (sharedIconName shared)
  , maybe "notification-indicator" id (sharedOverlay shared)
  , maybe (defaultBusName defaults) id (sharedBusName shared)
  , sharedLogLevel shared
  )

-- GitHub auth parsers

gitConfigGet :: String -> IO String
gitConfigGet key = do
  Right value <- runCommandFromPath ["git", "config", "--get", key]
  return value

githubTokenAuthParser :: Parser (IO GH.Auth)
githubTokenAuthParser = fmap (GH.OAuth . BS.pack . T.unpack . T.strip . T.pack) <$>
  (passGetMain <$> strOption
  (  long "token-pass"
  <> metavar "TOKEN-NAME"
  <> help "Use pass to get a token password to authenticate with github"
  ) <|>
  (gitConfigGet <$> strOption
  (  long "token-config"
  <> metavar "TOKEN-KEY"
  <> help "Get a github token using the provided git config key"
  )) <|>
  (return <$> strOption
  (  long "token-string"
  <> metavar "TOKEN"
  <> help "Provide the github token as a value"
  )))

githubConfigAuthParser :: Parser (IO GH.Auth)
githubConfigAuthParser =
  fmap githubAuthFromUsernamePassword <$> usernamePasswordParser
  where usernamePasswordParser =
          runGitConfigCommands <$> userOption <*> passwordOption
        userOption = strOption
                 (  long "config-user"
                 <> metavar "USER-KEY"
                 <> help "The git config key to use to get the github user"
                 )
        passwordOption = strOption
                 (  long "config-password"
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
  long "basic-auth"

githubAuthParser =
  githubTokenAuthParser <|> githubConfigAuthParser <|> githubConsoleAuthParser

githubSubParser :: Parser (IO GitHubConfig)
githubSubParser = fmap <$> helper <*> githubAuthParser
  where helper =
          flip GitHubConfig <$> option auto
            (  long "poll-interval"
            <> help "The amount of time to wait between refreshes of notification data"
            <> value 30
            <> metavar "SECONDS"
            )

-- Gmail parser

gmailSubParser :: Parser (IO GmailConfig)
gmailSubParser = buildConfig <$> clientIdOption <*> clientSecretOption
                             <*> optional tokenFileOption <*> pollIntervalOption
  where
    buildConfig cid secret tokenFile interval = return $ GmailConfig
      { gmailClientId = T.pack cid
      , gmailClientSecret = T.pack secret
      , gmailTokenFile = tokenFile
      , gmailRefreshSeconds = interval
      }
    clientIdOption = strOption
      (  long "client-id"
      <> metavar "CLIENT_ID"
      <> help "Google OAuth2 client ID for Gmail API"
      )
    clientSecretOption = strOption
      (  long "client-secret"
      <> metavar "CLIENT_SECRET"
      <> help "Google OAuth2 client secret for Gmail API"
      )
    tokenFileOption = strOption
      (  long "token-file"
      <> metavar "PATH"
      <> help "Path to store Gmail OAuth token (default: XDG config dir)"
      )
    pollIntervalOption = option auto
      (  long "poll-interval"
      <> help "Seconds between Gmail checks"
      <> value 30
      <> metavar "SECONDS"
      )

-- Gitea parser

giteaSubParser :: Parser (IO GiteaUpdaterConfig)
giteaSubParser = mkConfig <$> baseUrlOption <*> giteaTokenAuthParser <*> pollIntervalOption
  where
    baseUrlOption = strOption
      (  long "url"
      <> metavar "URL"
      <> help "The base URL of the Gitea instance (e.g. https://gitea.example.com)"
      )
    giteaTokenAuthParser = fmap (GiteaToken . BS.pack . T.unpack . T.strip . T.pack) <$>
      (passGetMain <$> strOption
      (  long "token-pass"
      <> metavar "TOKEN-NAME"
      <> help "Use pass to get a token to authenticate with Gitea"
      ) <|>
      (gitConfigGet <$> strOption
      (  long "token-config"
      <> metavar "TOKEN-KEY"
      <> help "Get a Gitea token using the provided git config key"
      )) <|>
      (return <$> strOption
      (  long "token-string"
      <> metavar "TOKEN"
      <> help "Provide the Gitea token as a value"
      )))
    pollIntervalOption = option auto
      (  long "poll-interval"
      <> help "The amount of time to wait between refreshes of notification data"
      <> value 30
      <> metavar "SECONDS"
      )
    mkConfig baseUrl getAuth interval = do
      auth <- getAuth
      return GiteaUpdaterConfig
        { giteaConfig = GiteaConfig { giteaAuth = auth, giteaBaseUrl = baseUrl }
        , giteaRefreshSeconds = interval
        }

-- | Each subcommand returns (IO updater, defaults).
type SubcommandResult = (IO (UpdateNotifications -> IO ()), SubcommandDefaults)

subcommandParser :: Parser SubcommandResult
subcommandParser = hsubparser
  (  command "github" (info
       ((,) <$> (fmap githubUpdaterNew <$> githubSubParser)
            <*> pure (SubcommandDefaults "github" "org.Github.Notifications"))
       (progDesc "GitHub notification tray icon"))
  <> command "gmail" (info
       ((,) <$> (fmap gmailUpdaterNew <$> gmailSubParser)
            <*> pure (SubcommandDefaults "gmail" "org.Gmail.Notifications"))
       (progDesc "Gmail notification tray icon"))
  <> command "gitea" (info
       ((,) <$> (fmap giteaUpdaterNew <$> giteaSubParser)
            <*> pure (SubcommandDefaults "gitea" "org.Gitea.Notifications"))
       (progDesc "Gitea notification tray icon"))
  <> command "sample" (info
       (pure (return sampleUpdater, SubcommandDefaults "github" "org.Sample.Notifications"))
       (progDesc "Sample tray icon for testing"))
  )

mkOverlayIconParams :: String -> String -> String -> String -> (UpdateNotifications -> IO ()) -> OverlayIconParams
mkOverlayIconParams themePath iconName overlayName busName updater = OverlayIconParams
  { iconName = iconName
  , iconPath = "/StatusNotifierItem"
  , iconDBusName = busName
  , iconThemePath = Just themePath
  , getOverlayName = \count -> return $ if count > 0 then T.pack overlayName else ""
  , runUpdater = updater
  }

run :: SharedOpts -> SubcommandResult -> IO ()
run shared (getUpdater, defaults) = do
  let (iconName, overlayName, busName, logLevel) = resolveOpts shared defaults
  logger <- getLogger "StatusNotifier.Item.Notifications"
  saveGlobalLogger $ setLevel logLevel logger
  dbusLogger <- getLogger "DBus"
  saveGlobalLogger $ setLevel logLevel dbusLogger
  dataDir <- getDataDir
  let themePath = dataDir </> "icons"
  updater <- getUpdater
  buildOverlayIcon $ mkOverlayIconParams themePath iconName overlayName busName updater

versionOption :: Parser (a -> a)
versionOption = infoOption
                (printf "notifications-tray-icon %s" versionString)
                (  long "version"
                <> (help $
                    printf "Show the version number of notifications-tray-icon (%s)"
                    versionString)
                )
  where versionString = showVersion version

parser :: Parser (IO ())
parser = run <$> sharedOptsParser <*> subcommandParser

main :: IO ()
main = do
  join $ execParser $ info (helper <*> versionOption <*> parser)
         (  fullDesc
         <> progDesc "Run a notification monitoring tray icon"
         )
  -- TODO: Come up with a better way to hang indefinitely
  void $ forever $ threadDelay 999999999999999999
