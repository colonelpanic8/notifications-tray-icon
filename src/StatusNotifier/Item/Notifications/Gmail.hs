{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StatusNotifier.Item.Notifications.Gmail
  ( GmailConfig(..)
  , defaultGmailConfig
  , setupGmailEnv
  , gmailUpdaterNew
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async (race)
import           Control.Concurrent.MVar as MV
import           Control.Exception (SomeException, try)
import           Control.Lens (view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Either (isRight)
import           Data.List (sort, (\\))
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GI.Dbusmenu
import           Gogol
                   ( Env
                   , LogLevel(..)
                   , newEnvWith
                   , newLogger
                   , send
                   , runResourceT
                   )
import           Gogol.Auth
                   ( OAuthClient(..)
                   , OAuthCode(..)
                   , Credentials(..)
                   , AuthorizedUser(..)
                   , ClientId(..)
                   , GSecret(..)
                   , installedApplication
                   , retrieveAuthFromStore
                   , authToAuthorizedUser
                   , saveAuthorizedUser
                   , fromFilePath
                   )
import           Gogol.Auth.InstalledApplication (AccessType(..), formAccessTypeURL)
import           Gogol.Env (envStore)
import           Gogol.Gmail
                   ( GmailUsersMessagesList(..)
                   , GmailUsersMessagesGet(..)
                   , GmailUsersMessagesModify(..)
                   , ListMessagesResponse(..)
                   , Message(..)
                   , MessagePart(..)
                   , MessagePartHeader(..)
                   , ModifyMessageRequest(..)
                   , UsersMessagesGetFormat(..)
                   , Gmail'Modify
                   , newGmailUsersMessagesList
                   , newGmailUsersMessagesGet
                   , newGmailUsersMessagesModify
                   , newModifyMessageRequest
                   )
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)
import           StatusNotifier.Item.Notifications.Util
import           System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(..))
import           System.FilePath ((</>))
import           System.IO (hFlush, stdout)
import           System.Log.Logger
import           Text.Printf

-- | Configuration for the Gmail notifier.
data GmailConfig = GmailConfig
  { gmailClientId     :: T.Text
  , gmailClientSecret :: T.Text
  , gmailTokenFile    :: Maybe FilePath
  , gmailRefreshSeconds :: Rational
  }

-- | Create a default config. You must provide client ID and secret.
defaultGmailConfig :: T.Text -> T.Text -> GmailConfig
defaultGmailConfig cid csecret = GmailConfig
  { gmailClientId       = cid
  , gmailClientSecret   = csecret
  , gmailTokenFile      = Nothing
  , gmailRefreshSeconds = 30
  }

gmailLog :: Priority -> String -> IO ()
gmailLog = logM "StatusNotifier.Item.Notifications.Gmail"

-- | Extract the message ID from a Message, avoiding conflict with Prelude.id.
messageId :: Message -> Maybe T.Text
messageId Message{id = mid} = mid

-- | A summary of a Gmail message for display.
data MessageSummary = MessageSummary
  { msId      :: T.Text
  , msSender  :: T.Text
  , msSubject :: T.Text
  , msSnippet :: T.Text
  } deriving (Show, Eq)

-- | Determine the token file path, defaulting to XDG config directory.
getTokenFilePath :: GmailConfig -> IO FilePath
getTokenFilePath GmailConfig{..} =
  case gmailTokenFile of
    Just fp -> return fp
    Nothing -> do
      dir <- getXdgDirectory XdgConfig "notifications-tray-icon"
      createDirectoryIfMissing True dir
      return $ dir </> "gmail-token.json"

-- | Set up a gogol Env for Gmail with Gmail'Modify scope.
--
-- If a saved token file exists, loads credentials from it.
-- Otherwise, runs the interactive OAuth2 Installed Application flow:
-- prints an authorization URL, prompts for the code, exchanges it,
-- and saves the resulting token.
setupGmailEnv :: GmailConfig -> IO (Env '[Gmail'Modify])
setupGmailEnv config@GmailConfig{..} = do
  tokenPath <- getTokenFilePath config
  mgr <- newManager tlsManagerSettings
  lgr <- newLogger Error stdout

  let client = OAuthClient
        { _clientId     = ClientId gmailClientId
        , _clientSecret = GSecret gmailClientSecret
        }

  exists <- doesFileExist tokenPath
  creds <- if exists
    then do
      gmailLog INFO $ printf "Loading saved token from %s" tokenPath
      fromFilePath tokenPath
    else do
      let authUrl = formAccessTypeURL client Offline (Proxy :: Proxy '[Gmail'Modify])
      putStrLn "Please open the following URL in your browser to authorize:"
      putStrLn $ T.unpack authUrl
      putStr "Enter the authorization code: "
      hFlush stdout
      code <- getLine
      let cred = installedApplication client (OAuthCode (T.pack code) :: OAuthCode '[Gmail'Modify])
      return cred

  env <- newEnvWith creds lgr mgr

  -- After initial exchange, save the authorized user token for future use
  when (not exists) $ do
    let store = view envStore env
    auth <- retrieveAuthFromStore store
    case authToAuthorizedUser auth of
      Right au -> do
        saveAuthorizedUser tokenPath True au
        gmailLog INFO $ printf "Token saved to %s" tokenPath
      Left err ->
        gmailLog WARNING $ printf "Could not save token: %s" (T.unpack err)

  return env

-- | Extract a header value by name from a Message's payload headers.
getHeader :: T.Text -> Message -> Maybe T.Text
getHeader headerName msg = do
  part <- payload (msg :: Message)
  hdrs <- headers (part :: MessagePart)
  let matching = filter (\h -> name (h :: MessagePartHeader) == Just headerName) hdrs
  case matching of
    (h:_) -> value (h :: MessagePartHeader)
    []    -> Nothing

-- | Build a MessageSummary from a full Message response.
getMessageSummary :: Message -> Maybe MessageSummary
getMessageSummary msg = do
  msgId <- messageId msg
  let sender  = fromMaybe "(unknown)" $ getHeader "From" msg
      subject = fromMaybe "(no subject)" $ getHeader "Subject" msg
      snip    = fromMaybe "" $ snippet (msg :: Message)
  return MessageSummary
    { msId      = msgId
    , msSender  = sender
    , msSubject = subject
    , msSnippet = snip
    }

-- | Shorten a sender string for display. Extracts just the name part
-- from "Name <email>" format, or returns as-is.
shortenSender :: T.Text -> T.Text
shortenSender s
  | T.null before = s
  | otherwise     = T.strip before
  where
    before = T.takeWhile (/= '<') s

-- | Format a notification summary string for display.
formatSummary :: MessageSummary -> String
formatSummary MessageSummary{..} =
  printf "%s - %s" (T.unpack $ shortenSender msSender) (T.unpack msSubject)

-- | Create a menu item for a single Gmail message.
makeGmailMenuItem :: Env '[Gmail'Modify] -> IO () -> MessageSummary -> IO Menuitem
makeGmailMenuItem env onMarkedRead summary@MessageSummary{..} = do
  menuItem <- menuitemNew
  let label = T.pack $ formatSummary summary
  textVariant <- liftIO $ toGVariant label
  menuitemPropertySetVariant menuItem "label" textVariant

  -- Sub-items: mark as read & open in browser
  markReadItem <- makeMenuItemWithLabel "Mark as read"
  onMenuitemItemActivated markReadItem $ const $ void $ forkIO $ do
    let modReq = newModifyMessageRequest
          { removeLabelIds = Just ["UNREAD"]
          }
    result <- try $ runResourceT $ send env (newGmailUsersMessagesModify msId modReq)
    case (result :: Either SomeException Message) of
      Right _  -> do
        gmailLog DEBUG $ printf "Marked message %s as read" (T.unpack msId)
        onMarkedRead
      Left err -> gmailLog ERROR $ printf "Failed to mark %s as read: %s" (T.unpack msId) (show err)
  menuitemChildAppend menuItem markReadItem

  openItem <- makeMenuItemWithLabel "Open in Gmail"
  onMenuitemItemActivated openItem $ const $
    void $ openURL $ "https://mail.google.com/mail/u/0/#inbox/" ++ T.unpack msId
  menuitemChildAppend menuItem openItem

  return menuItem

-- | The main updater function. Polls Gmail for unread inbox messages,
-- builds a tray menu, and sends desktop notifications for new arrivals.
--
-- Signature matches what OverlayIcon expects:
-- @GmailConfig -> (Int -> Menuitem -> IO ()) -> IO ()@
gmailUpdaterNew :: GmailConfig
                -> (Int -> Menuitem -> IO ())
                -> IO ()
gmailUpdaterNew config update = do
  env <- setupGmailEnv config

  summariesVar   <- MV.newMVar []
  errorVar       <- MV.newMVar (Nothing :: Maybe String)
  forceRefreshVar <- MV.newEmptyMVar

  let forceRefresh    = void $ MV.tryPutMVar forceRefreshVar ()
      delayedRefresh  = void $ forkIO $ threadDelay 1000000 >> forceRefresh
      refreshSeconds  = gmailRefreshSeconds config

      -- Fetch the list of unread inbox messages and retrieve their metadata.
      fetchMessages :: IO (Either String [MessageSummary])
      fetchMessages = do
        result <- try $ runResourceT $ do
          let listReq = newGmailUsersMessagesList
                { q = Just "is:unread in:inbox"
                , maxResults = 50
                }
          listResp <- send env listReq
          let msgStubs = fromMaybe [] $ messages (listResp :: ListMessagesResponse)
              msgIds   = mapMaybe (\m -> messageId m) msgStubs
          forM msgIds $ \mid' -> do
            let getReq = (newGmailUsersMessagesGet mid')
                  { format = UsersMessagesGetFormat_Metadata
                  , metadataHeaders = Just ["From", "Subject"]
                  }
            send env getReq
        case (result :: Either SomeException [Message]) of
          Left err -> return $ Left $ show err
          Right msgs -> return $ Right $ mapMaybe getMessageSummary msgs

      getCurrentSummaries = MV.readMVar summariesVar

      buildMenu = do
        sums <- getCurrentSummaries
        root <- menuitemNew
        mapM_ (\s -> makeGmailMenuItem env delayedRefresh s >>= menuitemChildAppend root) sums

        separatorItem <- menuitemNew
        menuitemPropertySet separatorItem MENUITEM_PROP_TYPE CLIENT_TYPES_SEPARATOR
        menuitemChildAppend root separatorItem

        openInboxItem <- makeMenuItemWithLabel "Open Gmail"
        onMenuitemItemActivated openInboxItem $ const $
          void $ openURL "https://mail.google.com"
        menuitemChildAppend root openInboxItem

        refreshItem <- makeMenuItemWithLabel "Refresh"
        onMenuitemItemActivated refreshItem $ const forceRefresh
        menuitemChildAppend root refreshItem

        return root

      updateVariables = do
        result <- fetchMessages
        case result of
          Left err -> do
            MV.modifyMVar_ errorVar (const $ return $ Just err)
            gmailLog ERROR $ printf "Error fetching Gmail: %s" err
            return (False, [])
          Right newSummaries -> do
            MV.modifyMVar_ errorVar (const $ return Nothing)
            MV.modifyMVar summariesVar $ \oldSummaries -> do
              let newIds = sort $ map msId newSummaries
                  oldIds = sort $ map msId oldSummaries
              return ( newSummaries
                     , ( newIds /= oldIds
                       , newIds \\ oldIds
                       )
                     )

      doUpdate = do
        newRoot <- buildMenu
        count <- length <$> getCurrentSummaries
        update count newRoot

      sendNotifications newIds = do
        sums <- getCurrentSummaries
        let getById i = filter ((== i) . msId) sums
        forM_ newIds $ \i ->
          case getById i of
            (s:_) -> sendNotification s
            []    -> return ()

      sendNotification summary =
        void $ runCommandFromPath
          [ "notify-send"
          , "--icon=mail-unread"
          , T.unpack $ shortenSender $ msSender summary
          , T.unpack $ msSubject summary
          ]

  -- Initial fetch and menu build
  void updateVariables
  doUpdate

  -- Polling loop
  void $ forkIO $ forever $ do
    forced <-
      isRight <$> race (threadDelay (floor $ refreshSeconds * 1000000))
                       (takeMVar forceRefreshVar)
    gmailLog DEBUG "Refreshing Gmail notifications"
    (menuNeedsRebuild, newIds) <- updateVariables
    sendNotifications newIds
    gmailLog DEBUG $ printf "Gmail rebuild needed: %s, force: %s"
                            (show menuNeedsRebuild) (show forced)
    when (forced || menuNeedsRebuild) doUpdate
