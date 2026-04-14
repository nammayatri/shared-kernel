module Kernel.External.GSTEInvoice.Interface
  ( module Reexport,
    module Kernel.External.GSTEInvoice.Interface,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CITypes
import qualified Kernel.External.GSTEInvoice.Interface.CharteredInfo as CharteredInfo
import Kernel.External.GSTEInvoice.Interface.Types
import Kernel.External.GSTEInvoice.Types as Reexport
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Text.Read (readMaybe)

-- | Authenticate with the GST e-invoice portal via the configured GSP.
authenticateEInvoice ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadFlow m
  ) =>
  GSTEInvoiceConfig ->
  m EInvoiceAuthResp
authenticateEInvoice serviceConfig = case serviceConfig of
  CharteredInfoEInvoiceConfig cfg -> CharteredInfo.authenticate cfg

-- | Generate an e-invoice IRN via the configured GSP.
generateEInvoice ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  GSTEInvoiceConfig ->
  Text ->
  CITypes.EInvoicePayload ->
  m EInvoiceGenerateResp
generateEInvoice serviceConfig authToken payload = case serviceConfig of
  CharteredInfoEInvoiceConfig cfg -> CharteredInfo.generateInvoice cfg authToken payload

-- | Extract GSTIN from the provider-agnostic config.
getGSTINFromConfig :: GSTEInvoiceConfig -> Text
getGSTINFromConfig (CharteredInfoEInvoiceConfig cfg) = cfg.gstin

-- | Parse token expiry text (e.g. "3600") to seconds. Defaults to 3600.
parseTokenExpiry :: Text -> Int
parseTokenExpiry t = fromMaybe 3600 (readMaybe (T.unpack t))

-- | Get a cached auth token from Redis, or authenticate and cache a new one.
--   TTL is set to (tokenExpiry - 60) seconds for safety.
getOrRefreshAuthToken ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  GSTEInvoiceConfig ->
  m Text
getOrRefreshAuthToken config = do
  let gstin = getGSTINFromConfig config
      redisKey = "GSTEInvoice:authToken:" <> gstin
  cached <- Redis.get redisKey
  case cached of
    Just token -> do
      logInfo $ "GSTEInvoice: Using cached auth token for GSTIN " <> gstin
      pure token
    Nothing -> do
      logInfo $ "GSTEInvoice: Fetching new auth token for GSTIN " <> gstin
      authResp <- authenticateEInvoice config
      let expirySeconds = parseTokenExpiry authResp.tokenExpiry
          ttl = max 60 (expirySeconds - 60)
      Redis.setExp redisKey authResp.authToken ttl
      pure authResp.authToken
