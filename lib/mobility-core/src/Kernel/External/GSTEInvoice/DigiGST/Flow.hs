{-# LANGUAGE PackageImports #-}

module Kernel.External.GSTEInvoice.DigiGST.Flow where

import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.External.GSTEInvoice.DigiGST.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (throwError)

-- POST /generateAccessToken.do with HTTP Basic auth and an empty body.
type AuthAPI =
  "generateAccessToken.do"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Post '[JSON] DigiGSTAuthResp

-- POST /generateEinvoice.do, authenticated by the idtoken header.
type GenerateInvoiceAPI =
  "generateEinvoice.do"
    :> Header' '[Required, Strict] "idtoken" Text
    :> Header "companyCode" Text
    :> Header "sourceId" Text
    :> Header' '[Required, Strict] "checksum" Text
    :> ReqBody '[JSON] DigiGSTInvoiceReq
    :> Post '[JSON] DigiGSTInvoiceResp

authClient :: Text -> Euler.EulerClient DigiGSTAuthResp
authClient = Euler.client (Proxy @AuthAPI)

generateInvoiceClient ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  DigiGSTInvoiceReq ->
  Euler.EulerClient DigiGSTInvoiceResp
generateInvoiceClient = Euler.client (Proxy @GenerateInvoiceAPI)

-- | Log in with the configured user and get an access token response.
authenticate ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  DigiGSTConfig ->
  m DigiGSTAuthResp
authenticate config = do
  pwd <- decrypt config.password
  let credentials = TE.decodeUtf8 (B64.encode (TE.encodeUtf8 (config.userName <> ":" <> pwd)))
  callDigiGSTAPI config.url (authClient ("Basic " <> credentials)) "digigst-generate-access-token" (Proxy @AuthAPI)

-- | Generate an e-invoice IRN via EY DigiGST with an id token from 'authenticate'.
generateInvoice ::
  ( Metrics.CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  DigiGSTConfig ->
  Text ->
  DigiGSTInvoiceReq ->
  m DigiGSTInvoiceResp
generateInvoice config token payload =
  callDigiGSTAPI
    config.url
    (generateInvoiceClient token config.companyCode config.sourceId "NOCHECKSUM" payload)
    "digigst-generate-invoice"
    (Proxy @GenerateInvoiceAPI)

callDigiGSTAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callDigiGSTAPI apiUrl eulerClient description proxy =
  callAPI apiUrl eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call DigiGST " <> description <> " API: " <> redactClientError (show err))
