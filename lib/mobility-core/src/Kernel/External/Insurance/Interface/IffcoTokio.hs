module Kernel.External.Insurance.Interface.IffcoTokio where

import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Insurance.IffcoTokio.Flow as IffcoFlow
import qualified Kernel.External.Insurance.IffcoTokio.Types as IffcoTypes
import Kernel.External.Insurance.Interface.Types
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Forkable (Forkable (..))
import Kernel.Utils.Common (MonadFlow)
import Kernel.Utils.Logging (logError, logInfo)
import Kernel.Utils.Servant.Client (HasRequestId)

registerHomeDeclaration ::
  ( Metrics.CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    Forkable m
  ) =>
  IffcoTypes.IffcoTokioConfig ->
  HomeDeclarationReq ->
  (Either Text HomeDeclarationAsyncResp -> m ()) ->
  m HomeDeclarationInstantResp
registerHomeDeclaration config req onComplete = do
  fork "iffco-Tokio-register-home-declaration" $ do
    logInfo $
      "IffcoTokio: firing RegisterHomeDeclaration for invoiceRequestNumber="
        <> req.invoiceRequestNumber
    result <- IffcoFlow.registerHomeDeclarationEither config (toIffcoReq req)
    case result of
      Left err -> do
        logError $
          "IffcoTokio: API call failed for invoiceRequestNumber="
            <> req.invoiceRequestNumber
            <> " error="
            <> err
        onComplete (Left err)
      Right resp -> do
        logInfo $
          "IffcoTokio: received response for invoiceRequestNumber="
            <> req.invoiceRequestNumber
            <> " status="
            <> resp.status
        onComplete (Right $ fromIffcoResp req.invoiceRequestNumber resp)
  return HomeDeclarationInstantResp {invoiceRequestNumber = req.invoiceRequestNumber}

toIffcoReq :: HomeDeclarationReq -> IffcoTypes.HomeDeclarationReq
toIffcoReq req =
  IffcoTypes.HomeDeclarationReq
    { insuredAddress = req.insuredAddress,
      insuredEmail = req.insuredEmail,
      insuredMobile = req.insuredMobile,
      insuredName = req.insuredName,
      invoiceDate = req.invoiceDate,
      invoiceRequestNumber = req.invoiceRequestNumber,
      ewCommencesOn = req.ewCommencesOn
    }

fromIffcoResp :: Text -> IffcoTypes.HomeDeclarationResp -> HomeDeclarationAsyncResp
fromIffcoResp invoiceReqNum resp =
  HomeDeclarationAsyncResp
    { invoiceRequestNumber = invoiceReqNum,
      certificateNumber = resp.certificateNumber,
      declarationId = resp.declarationId,
      status = resp.status
    }
