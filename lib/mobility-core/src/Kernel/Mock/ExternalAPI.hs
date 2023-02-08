{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.Mock.ExternalAPI where

import Kernel.Mock.App
import Kernel.Prelude (lookup)
import Kernel.Types.Common
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth (AuthenticatingEntity (getSignatureExpiry, getSigningKey))
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Control.Monad
import qualified Control.Monad.Catch as C
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt
import GHC.Records.Extra
import Network.HTTP.Client hiding (Proxy)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant.Client
import Universum

callBapAPI ::
  forall api a b e.
  ( Show a,
    HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text,
    HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b),
    HasLog e,
    HasField "authManager" e Manager
  ) =>
  BecknCallbackReq a ->
  MockM e ()
callBapAPI req = do
  let bapUrl = req.context.bap_uri
  logOutput INFO "calling BAP"
  callAPI @api bapUrl req

callAPI ::
  forall api a b e.
  ( Show a,
    HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text,
    HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b),
    HasLog e,
    HasField "authManager" e Manager
  ) =>
  BaseUrl ->
  BecknCallbackReq a ->
  MockM e ()
callAPI url req = do
  let clientFunc = client @api Proxy
      clientAction = clientFunc req
  logOutput INFO $ mconcat ["calling ", show req.context.action, "; url=", show url]
  logOutput DEBUG $ show req
  _ <- callClientM url clientAction
  pure ()

callClientM ::
  ( HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text,
    HasField "authManager" e Manager
  ) =>
  BaseUrl ->
  ClientM a ->
  MockM e a
callClientM url clientAction = do
  manager <- asks (.authManager)
  res <- liftIO $ runClientM clientAction $ mkClientEnv manager url
  either C.throwM pure res

prepareAuthManager ::
  AuthenticatingEntity cfg =>
  cfg ->
  [Text] ->
  Text ->
  Text ->
  (LogLevel -> Text -> IO ()) ->
  Http.ManagerSettings
prepareAuthManager appCfg signHeaders subscriberId uniqueKeyId logger =
  Http.tlsManagerSettings {Http.managerModifyRequest = doSignature}
  where
    doSignature :: Request -> IO Request
    doSignature req = do
      now <- getPOSIXTime
      let params = HttpSig.mkSignatureParams subscriberId uniqueKeyId now signatureExpiry HttpSig.Ed25519
      let body = getBody $ Http.requestBody req
      let bodyHash = HttpSig.becknSignatureHash body
      let headers = Http.requestHeaders req
      let signatureMsg = HttpSig.makeSignatureString params bodyHash headers
      logger DEBUG $ "Request body for signing: " +|| body ||+ ""
      logger DEBUG $ "Signature Message: " +|| signatureMsg ||+ ""
      let mbRes = foldM (addSignature bodyHash params headers) req signHeaders
      maybe (C.throwM $ InternalError $ "Could not add signature: " <> show params) pure mbRes
    getBody (Http.RequestBodyLBS body) = cs body
    getBody (Http.RequestBodyBS body) = body
    getBody _ = "<MISSING_BODY>"
    signPrivKey = getSigningKey appCfg
    signatureExpiry = getSignatureExpiry appCfg

    -- FIXME: we don't currently deal with Content-Length not being there (this is
    -- filled later, so we might need to have some special handling)
    addSignature bodyHash params headers req signHeader =
      let ciHeader = CI.mk $ encodeUtf8 signHeader
       in -- We check if the signHeader exists because `managerModifyRequest` might be
          -- called multiple times, so we already added it once, let's skip right over
          if isJust $ lookup ciHeader headers
            then Just req
            else do
              signature <- HttpSig.sign signPrivKey params bodyHash headers
              let headerVal = HttpSig.encode $ HttpSig.SignaturePayload signature params
              Just $ req {Http.requestHeaders = (ciHeader, headerVal) : headers}
