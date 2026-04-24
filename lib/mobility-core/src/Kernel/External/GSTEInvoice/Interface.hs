module Kernel.External.GSTEInvoice.Interface
  ( module Reexport,
    module Kernel.External.GSTEInvoice.Interface,
  )
where

import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CITypes
import qualified Kernel.External.GSTEInvoice.Interface.CharteredInfo as CharteredInfo
import Kernel.External.GSTEInvoice.Interface.Types
import Kernel.External.GSTEInvoice.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

-- | Authenticate with the GST e-invoice portal via the configured GSP.
authenticateEInvoice ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadFlow m
  ) =>
  GSTEInvoiceConfig ->
  m EInvoiceAuthResp
authenticateEInvoice serviceConfig = do
  logInfo $ "GSTEInvoice.authenticateEInvoice: calling GSP with config=" <> show serviceConfig
  resp <- case serviceConfig of
    CharteredInfoEInvoiceConfig cfg -> CharteredInfo.authenticate cfg
  logInfo $ "GSTEInvoice.authenticateEInvoice: received response=" <> show resp
  pure resp

-- | Generate an e-invoice IRN via the configured GSP.
generateEInvoice ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadFlow m
  ) =>
  GSTEInvoiceConfig ->
  Text ->
  CITypes.EInvoicePayload ->
  m EInvoiceGenerateResp
generateEInvoice serviceConfig authToken payload = do
  logInfo $ "GSTEInvoice.generateEInvoice: calling GSP with payload=" <> show payload
  resp <- case serviceConfig of
    CharteredInfoEInvoiceConfig cfg -> CharteredInfo.generateInvoice cfg authToken payload
  logInfo $ "GSTEInvoice.generateEInvoice: received response=" <> show resp
  pure resp
