module Gitea.API
  ( GiteaAuth(..)
  , GiteaConfig(..)
  , GiteaNotification(..)
  , GiteaNotificationRepo(..)
  , GiteaNotificationSubject(..)
  , getNotifications
  , markNotificationRead
  , markAllNotificationsRead
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Network.HTTP.Simple
import           Network.HTTP.Types

data GiteaAuth = GiteaToken BS.ByteString

data GiteaConfig = GiteaConfig
  { giteaAuth :: GiteaAuth
  , giteaBaseUrl :: String
  }

data GiteaNotificationSubject = GiteaNotificationSubject
  { giteaSubjectTitle :: T.Text
  , giteaSubjectUrl :: Maybe T.Text
  , giteaSubjectHtmlUrl :: Maybe T.Text
  , giteaSubjectLatestCommentHtmlUrl :: Maybe T.Text
  , giteaSubjectType :: T.Text
  , giteaSubjectState :: Maybe T.Text
  } deriving (Show)

instance FromJSON GiteaNotificationSubject where
  parseJSON = withObject "GiteaNotificationSubject" $ \v ->
    GiteaNotificationSubject
      <$> v .: "title"
      <*> v .:? "url"
      <*> v .:? "html_url"
      <*> v .:? "latest_comment_html_url"
      <*> v .: "type"
      <*> v .:? "state"

data GiteaNotification = GiteaNotification
  { giteaNotificationId :: Int
  , giteaNotificationRepo :: GiteaNotificationRepo
  , giteaNotificationSubject :: GiteaNotificationSubject
  , giteaNotificationUnread :: Bool
  } deriving (Show)

data GiteaNotificationRepo = GiteaNotificationRepo
  { giteaRepoFullName :: T.Text
  , giteaRepoHtmlUrl :: T.Text
  } deriving (Show)

instance FromJSON GiteaNotificationRepo where
  parseJSON = withObject "GiteaNotificationRepo" $ \v ->
    GiteaNotificationRepo
      <$> v .: "full_name"
      <*> v .: "html_url"

instance FromJSON GiteaNotification where
  parseJSON = withObject "GiteaNotification" $ \v ->
    GiteaNotification
      <$> v .: "id"
      <*> v .: "repository"
      <*> v .: "subject"
      <*> v .: "unread"

authHeaders :: GiteaAuth -> RequestHeaders
authHeaders (GiteaToken token) = [("Authorization", "token " <> token)]

giteaRequest :: GiteaConfig -> String -> Request
giteaRequest GiteaConfig { giteaAuth = auth, giteaBaseUrl = baseUrl } path =
  setRequestHeaders (authHeaders auth <> [("Accept", "application/json")])
  $ parseRequest_ (baseUrl <> "/api/v1" <> path)

getNotifications :: GiteaConfig -> IO (Either String [GiteaNotification])
getNotifications config = do
  let request = giteaRequest config "/notifications"
  response <- httpLBS request
  let status = getResponseStatusCode response
  return $ if status == 200
    then case eitherDecode (getResponseBody response) of
      Left err -> Left $ "JSON decode error: " <> err
      Right ns -> Right ns
    else Left $ "HTTP error: " <> show status

markNotificationRead :: GiteaConfig -> Int -> IO (Either String ())
markNotificationRead config notifId = do
  let request = setRequestMethod "PATCH"
              $ giteaRequest config ("/notifications/threads/" <> show notifId)
  response <- httpLBS request
  let status = getResponseStatusCode response
  return $ if status >= 200 && status < 300
    then Right ()
    else Left $ "HTTP error: " <> show status

markAllNotificationsRead :: GiteaConfig -> IO (Either String ())
markAllNotificationsRead config = do
  let request = setRequestMethod "PUT"
              $ giteaRequest config "/notifications"
  response <- httpLBS request
  let status = getResponseStatusCode response
  return $ if status >= 200 && status < 300
    then Right ()
    else Left $ "HTTP error: " <> show status
