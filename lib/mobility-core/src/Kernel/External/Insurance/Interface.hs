module Kernel.External.Insurance.Interface
  ( module Reexport,
    module Kernel.External.Insurance.Interface,
  )
where

import qualified Kernel.External.Insurance.IffcoTokio.Types as IffcoTokioTypes
import qualified Kernel.External.Insurance.Interface.Acko as Acko
import qualified Kernel.External.Insurance.Interface.IffcoTokio as IffcoTokio
import Kernel.External.Insurance.Interface.Types
import Kernel.External.Insurance.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common

createInsurance ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  InsuranceConfig ->
  InsuranceRequest ->
  m InsuranceResponse
createInsurance serviceConfig req = case serviceConfig of
  AckoInsuranceConfig cfg -> Acko.createInsurance cfg req
  IffcoTokioInsuranceConfig _ -> throwError $ InternalError "createInsurance is not supported for IffcoTokio; use registerHomeDeclaration instead"

-- | Fire-and-return home insurance declaration for IFFCO Tokio.
-- The call is executed in a background thread; the caller receives
-- the correlation |invoiceRequestNumber| immediately and must handle
-- the async result inside the |onComplete| callback (e.g. by persisting it).
registerHomeDeclaration ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    Forkable m
  ) =>
  IffcoTokioTypes.IffcoTokioConfig ->
  HomeDeclarationReq ->
  (Either Text HomeDeclarationAsyncResp -> m ()) ->
  m HomeDeclarationInstantResp
registerHomeDeclaration = IffcoTokio.registerHomeDeclaration
