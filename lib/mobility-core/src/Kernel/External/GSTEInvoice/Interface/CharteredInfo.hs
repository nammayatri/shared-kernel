module Kernel.External.GSTEInvoice.Interface.CharteredInfo where

import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Flow as Flow
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CITypes
import Kernel.External.GSTEInvoice.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (MonadFlow, throwError)
import Kernel.Utils.Servant.Client (HasRequestId)

authenticate ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    MonadFlow m
  ) =>
  CITypes.CharteredInfoConfig ->
  m EInvoiceAuthResp
authenticate config = do
  resp <- Flow.authenticate config
  case resp.authData of
    Nothing ->
      throwError $ InternalError "Chartered Info auth failed: no Data in response"
    Just d ->
      return
        EInvoiceAuthResp
          { authToken = d.authToken,
            tokenExpiry = d.tokenExpiry
          }

generateInvoice ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CITypes.CharteredInfoConfig ->
  Text ->
  CITypes.EInvoicePayload ->
  m EInvoiceGenerateResp
generateInvoice config authToken payload = do
  resp <- Flow.generateInvoice config authToken payload
  return $ fromCIInvoiceResp resp

-- ---------------------------------------------------------------------------
-- Internal transformers
-- ---------------------------------------------------------------------------

fromCIInvoiceResp :: CITypes.InvoiceResp -> EInvoiceGenerateResp
fromCIInvoiceResp resp =
  EInvoiceGenerateResp
    { status = resp.status,
      irn = resp.invoiceData >>= \d -> Just d.irn,
      ackNo = resp.invoiceData >>= \d -> Just d.ackNo,
      ackDt = resp.invoiceData >>= \d -> Just d.ackDt,
      signedInvoice = resp.invoiceData >>= (.signedInvoice),
      signedQRCode = resp.invoiceData >>= (.signedQRCode),
      errors = fmap (map fromCIError) resp.errorDetails,
      infoDtls = fmap (map fromCIInfoDtl) resp.infoDtls
    }

fromCIError :: CITypes.EInvoiceError -> EInvoiceErrorDetail
fromCIError err =
  EInvoiceErrorDetail
    { errorCode = err.errorCode,
      errorMessage = err.errorMessage
    }

fromCIInfoDtl :: CITypes.InfoDtl -> EInvoiceInfoDetail
fromCIInfoDtl info =
  EInvoiceInfoDetail
    { infoCode = info.infCd,
      description = info.desc
    }
