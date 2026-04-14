module Kernel.External.GSTEInvoice.CharteredInfo.Flow where

import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.External.GSTEInvoice.CharteredInfo.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Network.HTTP.Client.TLS as HttpTLS
import Servant hiding (throwError)
import qualified Servant.Client as SC

-- ---------------------------------------------------------------------------
-- Servant API types
-- ---------------------------------------------------------------------------

-- Auth: GET /eivital/dec/v1.04/auth?aspid=...&password=...&Gstin=...&user_name=...&eInvPwd=...
type AuthAPI =
  "eivital"
    :> "dec"
    :> "v1.04"
    :> "auth"
    :> QueryParam "aspid" Text
    :> QueryParam "password" Text
    :> QueryParam "Gstin" Text
    :> QueryParam "user_name" Text
    :> QueryParam "eInvPwd" Text
    :> Get '[JSON] AuthResp

-- Invoice: POST /eicore/dec/v1.03/Invoice?aspid=...&password=...&Gstin=...&AuthToken=...&user_name=...&eInvPwd=...
type GenerateInvoiceAPI =
  "eicore"
    :> "dec"
    :> "v1.03"
    :> "Invoice"
    :> QueryParam "aspid" Text
    :> QueryParam "password" Text
    :> QueryParam "Gstin" Text
    :> QueryParam "AuthToken" Text
    :> QueryParam "user_name" Text
    :> QueryParam "eInvPwd" Text
    :> ReqBody '[JSON] EInvoicePayload
    :> Post '[JSON] InvoiceResp

-- ---------------------------------------------------------------------------
-- Servant clients
-- ---------------------------------------------------------------------------

authClient ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Euler.EulerClient AuthResp
authClient = Euler.client (Proxy @AuthAPI)

generateInvoiceClient ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  EInvoicePayload ->
  Euler.EulerClient InvoiceResp
generateInvoiceClient = Euler.client (Proxy @GenerateInvoiceAPI)

-- ---------------------------------------------------------------------------
-- Flow functions
-- ---------------------------------------------------------------------------

-- | Authenticate with the Chartered Info GSP and obtain an auth token.
authenticate ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CharteredInfoConfig ->
  m AuthResp
authenticate config = do
  decryptedPassword <- decrypt config.password
  decryptedEInvPwd <- decrypt config.eInvPwd
  let eulerClient =
        authClient
          (Just config.aspId)
          (Just decryptedPassword)
          (Just config.gstin)
          (Just config.userName)
          (Just decryptedEInvPwd)
  callCharteredInfoAPI config.authUrl eulerClient "chartered-info-auth" (Proxy @AuthAPI)

-- | Generate an e-invoice IRN via the Chartered Info GSP.
generateInvoice ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CharteredInfoConfig ->
  Text ->
  EInvoicePayload ->
  m InvoiceResp
generateInvoice config authToken payload = do
  decryptedPassword <- decrypt config.password
  decryptedEInvPwd <- decrypt config.eInvPwd
  let eulerClient =
        generateInvoiceClient
          (Just config.aspId)
          (Just decryptedPassword)
          (Just config.gstin)
          (Just authToken)
          (Just config.userName)
          (Just decryptedEInvPwd)
          payload
  callCharteredInfoAPI config.invoiceUrl eulerClient "chartered-info-generate-invoice" (Proxy @GenerateInvoiceAPI)

-- ---------------------------------------------------------------------------
-- Internal helper
-- ---------------------------------------------------------------------------

callCharteredInfoAPI :: (MonadFlow m, HasRequestId r, MonadReader r m) => CallAPI' m r api res res
callCharteredInfoAPI url eulerClient description proxy = do
  callAPI url eulerClient description proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call Chartered Info " <> description <> " API: " <> show err)

-- ---------------------------------------------------------------------------
-- Direct IO test functions (bypass EulerHS monad stack)
-- These use servant-client directly and are intended for standalone testing.
-- ---------------------------------------------------------------------------

-- | Servant-client API types for direct testing
type AuthClientAPI =
  "eivital"
    :> "dec"
    :> "v1.04"
    :> "auth"
    :> QueryParam "aspid" Text
    :> QueryParam "password" Text
    :> QueryParam "Gstin" Text
    :> QueryParam "user_name" Text
    :> QueryParam "eInvPwd" Text
    :> Get '[JSON] AuthResp

type GenerateInvoiceClientAPI =
  "eicore"
    :> "dec"
    :> "v1.03"
    :> "Invoice"
    :> QueryParam "aspid" Text
    :> QueryParam "password" Text
    :> QueryParam "Gstin" Text
    :> QueryParam "AuthToken" Text
    :> QueryParam "user_name" Text
    :> QueryParam "eInvPwd" Text
    :> ReqBody '[JSON] EInvoicePayload
    :> Post '[JSON] InvoiceResp

authServantClient ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  SC.ClientM AuthResp
authServantClient = SC.client (Proxy @AuthClientAPI)

generateInvoiceServantClient ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  EInvoicePayload ->
  SC.ClientM InvoiceResp
generateInvoiceServantClient = SC.client (Proxy @GenerateInvoiceClientAPI)

-- | Test authenticate in plain IO — calls the real API using servant-client.
authenticateIO ::
  Text -> -- aspId
  Text -> -- password (plaintext)
  Text -> -- gstin
  Text -> -- userName
  Text -> -- eInvPwd (plaintext)
  BaseUrl -> -- authUrl
  IO (Either SC.ClientError AuthResp)
authenticateIO aspId' password' gstin' userName' eInvPwd' authUrl' = do
  manager <- HttpTLS.newTlsManager
  let env = SC.mkClientEnv manager authUrl'
  SC.runClientM
    (authServantClient (Just aspId') (Just password') (Just gstin') (Just userName') (Just eInvPwd'))
    env

-- | Test generateInvoice in plain IO — calls the real API using servant-client.
generateInvoiceIO ::
  Text -> -- aspId
  Text -> -- password (plaintext)
  Text -> -- gstin
  Text -> -- authToken
  Text -> -- userName
  Text -> -- eInvPwd (plaintext)
  BaseUrl -> -- invoiceUrl
  EInvoicePayload -> -- payload
  IO (Either SC.ClientError InvoiceResp)
generateInvoiceIO aspId' password' gstin' authToken' userName' eInvPwd' invoiceUrl' payload = do
  manager <- HttpTLS.newTlsManager
  let env = SC.mkClientEnv manager invoiceUrl'
  SC.runClientM
    (generateInvoiceServantClient (Just aspId') (Just password') (Just gstin') (Just authToken') (Just userName') (Just eInvPwd') payload)
    env
