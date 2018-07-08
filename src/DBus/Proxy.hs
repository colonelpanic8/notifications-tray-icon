{-# LANGUAGE OverloadedStrings #-}
module DBus.Proxy where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           DBus
import           DBus.Client
import           DBus.Internal.Message
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import qualified DBus.TH as DBusTH
import           Data.Coerce
import           Data.Maybe
import           System.Log.Logger

data ProxyOptions = ProxyOptions Client BusName ObjectPath InterfaceName

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

proxyAll :: Client -> BusName -> ObjectPath -> ObjectPath -> IO ()
proxyAll client busName pathToProxy registrationPath = either print return =<< (runExceptT $ do
  let introspectionCall =
        (methodCall pathToProxy
                     (interfaceName_ "org.freedesktop.DBus.Introspectable")
                     "Introspect")
                     { methodCallDestination = Just busName }
  returnValue <- ExceptT $ left methodErrorName <$> call client introspectionCall
  obj <- ExceptT $ return $ do
    xmlString <- maybeToEither errorInvalidParameters $
                 listToMaybe (methodReturnBody returnValue) >>= fromVariant
    maybeToEither errorFailed $ I.parseXML "/" xmlString
  let interfaces = I.objectInterfaces obj
  lift $ do
    logM "DBus.Proxy" DEBUG $ show obj
    when (length interfaces < 1) $
         logM "DBus.Proxy" WARNING "No interfaces found when attempting to proxy"
    mapM_ runInterface interfaces)
  where runInterface interface = do
           let proxyOptions = ProxyOptions client busName pathToProxy $
                              I.interfaceName interface
           buildAndRegisterInterface proxyOptions interface registrationPath

buildAndRegisterInterface :: ProxyOptions -> I.Interface -> ObjectPath -> IO ()
buildAndRegisterInterface
   options@(ProxyOptions client busName path interfaceName)
   introspectionInterface registrationPath = do
     let interface = buildInterface options introspectionInterface
     export client registrationPath interface
     forwardSignals options

buildInterface :: ProxyOptions -> I.Interface -> Interface
buildInterface options I.Interface
                 { I.interfaceName = name
                 , I.interfaceMethods = methods
                 , I.interfaceProperties = properties
                 } = Interface
  { interfaceName = name
  , interfaceMethods = map (buildMethod options) $ methods
  , interfaceProperties = map (buildProperty options) properties
  , interfaceSignals = []
  }

buildReply :: Either MethodError MethodReturn -> Reply
buildReply (Left MethodError { methodErrorName = errorName
                             , methodErrorBody = body
                             }) = ReplyError errorName body
buildReply (Right MethodReturn { methodReturnBody = body }) =
  ReplyReturn body

buildMethod :: ProxyOptions -> I.Method -> Method
buildMethod (ProxyOptions client busName path interfaceName)
            introspectionMethod = Method
  { methodName = I.methodName introspectionMethod
  , inSignature = T.Signature []            -- TODO: make signature accurate
  , outSignature = T.Signature []
  , methodHandler = lift . handler
  }
  where handler theMethodCall =
           buildReply <$> call client
                     theMethodCall { methodCallPath = path
                                , methodCallDestination = Just busName
                                }

buildProperty :: ProxyOptions -> I.Property -> Property
buildProperty (ProxyOptions client busName path interfaceName)
              introspectionProperty = Property
  { propertyName = propName
  , propertyType = T.TypeVariant -- TODO: make this accurate
  , propertyGetter = Just $ getter
  , propertySetter = Just $ void . setter
  }
  where propName = memberName_ $ I.propertyName introspectionProperty
        baseMethodCall = (methodCall path interfaceName propName)
                         { methodCallDestination = Just busName}
        getter = either (const $ toVariant ("" :: String)) id <$> getProperty client baseMethodCall
        setter = setProperty client baseMethodCall

forwardSignals :: ProxyOptions -> IO ()
forwardSignals (ProxyOptions client busName path interfaceName) = do
  -- TODO: Handle name owner changes?
  let forwardSignal = emit client
      updateSignalForwarding =
          void $ runExceptT $ do
            nameOwnerString <- ExceptT $ DBusTH.getNameOwner client $ coerce busName
            let matchRule = matchAny
                            { matchPath = Just path
                            , matchInterface = Just interfaceName
                            , matchSender = Just $ busName_ nameOwnerString
                            }
            lift $ addMatch client matchRule forwardSignal
  updateSignalForwarding
