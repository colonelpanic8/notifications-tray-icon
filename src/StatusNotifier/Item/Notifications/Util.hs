module StatusNotifier.Item.Notifications.Util where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text as T
import           GI.Dbusmenu
import           System.Exit (ExitCode (..))
import           System.Log.Logger
import qualified System.Process as P
import           Text.Printf
import           Text.Regex

fieldRegex :: Regex
fieldRegex = mkRegexWithOpts "^(.*?): *(.*?)$" True True

runCommandFromPath :: MonadIO m => [String] -> m (Either String String)
runCommandFromPath = runCommand "/usr/bin/env"

-- | Run the provided command with the provided arguments.
runCommand :: MonadIO m => FilePath -> [String] -> m (Either String String)
runCommand cmd args = liftIO $ do
  (ecode, stdout, stderr) <- P.readProcessWithExitCode cmd args ""
  logM "System.Taffybar.Util" INFO $
       printf "Running command %s with args %s" (show cmd) (show args)
  return $ case ecode of
    ExitSuccess -> Right stdout
    ExitFailure exitCode -> Left $ printf "Exit code %s: %s " (show exitCode) stderr

passGet :: MonadIO m => String -> m (Either String (String, [(String, String)]))
passGet credentialName =
  right (getPassComponents . lines) <$>
        runCommandFromPath ["pass", "show", credentialName]
  where getPassComponents passLines =
          let entries =
                map buildEntry $ catMaybes $
                    matchRegex fieldRegex <$> tail passLines
              buildEntry [fieldName, fieldValue] = (fieldName, fieldValue)
              buildEntry _ = ("", "")
          in (head passLines, entries)

passGetMain :: MonadIO m => String -> m String
passGetMain name = do
  Right (value, _) <- passGet name
  return value

xdgOpen :: MonadIO m => [String] -> m (Either String String)
xdgOpen args = runCommandFromPath ("xdg-open":args)

openURL :: MonadIO m => String -> m (Either String String)
openURL = xdgOpen . return

makeMenuItemWithLabel :: T.Text -> IO Menuitem
makeMenuItemWithLabel text = do
  menuItem <- menuitemNew
  textVariant <- liftIO $ toGVariant text
  menuitemPropertySetVariant menuItem "label" textVariant
  return menuItem

makeSeparatorMenuItem :: IO Menuitem
makeSeparatorMenuItem = undefined
