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
