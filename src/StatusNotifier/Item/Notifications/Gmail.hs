{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import           Control.Exception (SomeException, bracket, try)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import           Data.Either (isRight)
import           Data.List (sort, (\\))
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
                   , ClientId(..)
                   , GSecret(..)
                   , saveAuthorizedUser
                   , fromFilePath
                   )
import           Gogol.Auth.Scope (KnownScopes(..), queryEncodeScopes)
import           Gogol.Internal.Auth
                   ( AuthorizedUser(..)
                   , OAuthToken(..)
                   , accountsURL
                   , refreshRequest
                   , textBody
                   , tokenRequest
                   )
import qualified Gogol.Internal.Logger as GLogger
import           Gogol.Prelude (toQueryParam)
import           Gogol.Gmail
                   ( GmailUsersMessagesList(..)
                   , GmailUsersMessagesGet(..)
                   , GmailUsersMessagesModify(..)
                   , GmailUsersMessagesBatchModify(..)
                   , BatchModifyMessagesRequest(..)
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
                   , newGmailUsersMessagesBatchModify
                   , newBatchModifyMessagesRequest
                   , newModifyMessageRequest
                   )
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)
import qualified Network.HTTP.Client as Client
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
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

-- | Build an OAuth2 authorization URL with a loopback redirect URI.
loopbackAuthURL :: OAuthClient -> Int -> T.Text
loopbackAuthURL client port =
  accountsURL
  <> "?response_type=code"
  <> "&client_id=" <> toQueryParam (_clientId client)
  <> "&redirect_uri=" <> loopbackRedirectURI port
  <> "&scope=" <> T.decodeUtf8 (queryEncodeScopes (scopeVals (Proxy :: Proxy '[Gmail'Modify])))
  <> "&access_type=offline"

loopbackRedirectURI :: Int -> T.Text
loopbackRedirectURI port = "http://localhost:" <> T.pack (show port)

-- | Exchange an authorization code for a token using the loopback redirect URI.
exchangeCodeLoopback
  :: OAuthClient -> OAuthCode s -> Int
  -> GLogger.Logger
  -> Client.Manager -> IO (OAuthToken s)
exchangeCodeLoopback client code port =
  refreshRequest $
    tokenRequest
      { Client.requestBody = textBody $
          "grant_type=authorization_code"
          <> "&client_id=" <> toQueryParam (_clientId client)
          <> "&client_secret=" <> toQueryParam (_clientSecret client)
          <> "&code=" <> toQueryParam code
          <> "&redirect_uri=" <> loopbackRedirectURI port
      }

-- | Start a temporary HTTP server, wait for the OAuth redirect, extract the code.
waitForOAuthRedirect :: Int -> IO T.Text
waitForOAuthRedirect port = do
  let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 1
    (conn, _) <- accept sock
    request <- recv conn 4096
    let code = extractCodeFromRequest request
        responseBody = case code of
              Just _ -> "Authorization successful! You can close this window."
              Nothing -> "Authorization failed. No code received."
        response = BS8.unlines
              [ "HTTP/1.1 200 OK"
              , "Content-Type: text/plain"
              , "Connection: close"
              , ""
              , responseBody
              ]
    sendAll conn response
    close conn
    case code of
      Just c  -> return c
      Nothing -> fail "No authorization code received in OAuth redirect"

-- | Extract the 'code' query parameter from an HTTP GET request.
extractCodeFromRequest :: BS8.ByteString -> Maybe T.Text
extractCodeFromRequest req = do
  firstLine <- case BS8.lines req of
    (l:_) -> Just l
    []    -> Nothing
  -- Parse "GET /path?code=xxx&... HTTP/1.1"
  path <- case BS8.words firstLine of
    (_:p:_) -> Just p
    _       -> Nothing
  let query = BS8.dropWhile (/= '?') path
      params = parseQueryParams (BS8.drop 1 query)  -- drop the '?'
  lookup "code" params

-- | Parse query parameters from a query string like "code=xxx&scope=yyy"
parseQueryParams :: BS8.ByteString -> [(BS8.ByteString, T.Text)]
parseQueryParams qs =
  [ (key, T.decodeUtf8 val)
  | part <- BS8.split '&' qs
  , let (key, rest) = BS8.break (== '=') part
        val = BS8.drop 1 rest  -- drop the '='
  , not (BS8.null key)
  ]

-- | Set up a gogol Env for Gmail with Gmail'Modify scope.
--
-- If a saved token file exists, loads credentials from it.
-- Otherwise, runs the OAuth2 loopback flow: opens the browser,
-- captures the redirect on localhost, exchanges the code, and saves the token.
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
  if exists
    then do
      gmailLog INFO $ printf "Loading saved token from %s" tokenPath
      creds <- fromFilePath tokenPath
      newEnvWith creds lgr mgr
    else do
      let port = 8914
          authUrl = loopbackAuthURL client port
      gmailLog INFO "Starting OAuth2 loopback flow"
      putStrLn "Opening browser for Gmail authorization..."
      void $ xdgOpen [T.unpack authUrl]
      putStrLn $ "Waiting for authorization redirect on port " ++ show port ++ "..."
      code <- waitForOAuthRedirect port
      gmailLog INFO "Authorization code received"

      -- Exchange the code ourselves (with matching loopback redirect_uri)
      token <- exchangeCodeLoopback client
                 (OAuthCode code :: OAuthCode '[Gmail'Modify])
                 port lgr mgr

      -- Build an AuthorizedUser from the exchange result and save it
      let authorizedUser = AuthorizedUser
            { _userId = _clientId client
            , _userRefresh = case _tokenRefresh token of
                Just r  -> r
                Nothing -> error "OAuth token exchange did not return a refresh token"
            , _userSecret = _clientSecret client
            }
      saveAuthorizedUser tokenPath True authorizedUser
      gmailLog INFO $ printf "Token saved to %s" tokenPath

      -- Now load from the saved file to create the Env
      creds <- fromFilePath tokenPath
      newEnvWith creds lgr mgr

-- | Extract a header value by name from a Message's payload headers.
getHeader :: T.Text -> Message -> Maybe T.Text
getHeader headerName msg = do
  part <- msg.payload
  hdrs <- part.headers
  let matching = filter (\h -> h.name == Just headerName) hdrs
  case matching of
    (h:_) -> h.value
    []    -> Nothing

-- | Build a MessageSummary from a full Message response.
getMessageSummary :: Message -> Maybe MessageSummary
getMessageSummary msg = do
  msgId <- messageId msg
  let sender  = fromMaybe "(unknown)" $ getHeader "From" msg
      subject = fromMaybe "(no subject)" $ getHeader "Subject" msg
      snip    = fromMaybe "" msg.snippet
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
    let modReq = (newModifyMessageRequest :: ModifyMessageRequest)
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
          let msgStubs = fromMaybe [] listResp.messages
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

        markAllReadItem <- makeMenuItemWithLabel "Mark all as read"
        onMenuitemItemActivated markAllReadItem $ const $ void $ forkIO $ do
          ids <- map msId <$> getCurrentSummaries
          unless (null ids) $ do
            let batchReq = newBatchModifyMessagesRequest
                  { ids = Just ids
                  , removeLabelIds = Just ["UNREAD"]
                  }
            result <- try $ runResourceT $ send env (newGmailUsersMessagesBatchModify batchReq)
            case (result :: Either SomeException ()) of
              Right () -> do
                gmailLog DEBUG "Marked all messages as read"
                delayedRefresh
              Left err -> gmailLog ERROR $ printf "Failed to mark all as read: %s" (show err)
        menuitemChildAppend root markAllReadItem

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
    result <- try $ do
      forced <-
        isRight <$> race (threadDelay (floor $ refreshSeconds * 1000000))
                         (takeMVar forceRefreshVar)
      gmailLog DEBUG "Refreshing Gmail notifications"
      (menuNeedsRebuild, newIds) <- updateVariables
      sendNotifications newIds
      gmailLog DEBUG $ printf "Gmail rebuild needed: %s, force: %s"
                              (show menuNeedsRebuild) (show forced)
      when (forced || menuNeedsRebuild) doUpdate
    case (result :: Either SomeException ()) of
      Right () -> return ()
      Left err -> gmailLog ERROR $ printf "Exception in polling loop: %s" (show err)
