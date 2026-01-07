{-# LANGUAGE PackageImports #-}

module Kernel.External.Tokenize.Tten.Flow where

import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as DE
import EulerHS.Types (EulerClient, client)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Tokenize.Tten.Types as TtenTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type GenerateTokenAPI =
  "api"
    :> "auth"
    :> "generateToken"
    :> Header "Authorization" Text
    :> Post '[JSON] TtenTypes.GenerateTokenResp

generateTokenClient :: Maybe Text -> EulerClient TtenTypes.GenerateTokenResp
generateTokenClient = client (Proxy :: Proxy GenerateTokenAPI)

generateToken ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  TtenTypes.TtenTokenizationConfig ->
  m TtenTypes.GenerateTokenResp
generateToken TtenTypes.TtenTokenizationConfig {..} = do
  password' <- decrypt password
  let credentials = username <> ":" <> password'
      encodedCredentials = decodeUtf8 $ Base64.encode $ DE.encodeUtf8 credentials
      authHeader = "Basic " <> encodedCredentials
  callAPI' Nothing url (generateTokenClient (Just authHeader)) "TTEN-GENERATE_TOKEN-API" (Proxy @GenerateTokenAPI) >>= checkGenerateTokenResponse url

checkGenerateTokenResponse ::
  ( HasCallStack,
    MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Either ClientError TtenTypes.GenerateTokenResp ->
  m TtenTypes.GenerateTokenResp
checkGenerateTokenResponse url resp = fromEitherM (ttenError url) resp >>= validateGenerateTokenResponse

validateGenerateTokenResponse :: (MonadThrow m, Log m) => TtenTypes.GenerateTokenResp -> m TtenTypes.GenerateTokenResp
validateGenerateTokenResponse resp = do
  logDebug $ "TTEN Generate Token Response: " <> show resp
  pure resp

ttenError :: BaseUrl -> ClientError -> ExternalAPICallError
ttenError = ExternalAPICallError (Just "TTEN_API_ERROR")
