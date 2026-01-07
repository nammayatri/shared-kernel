module Kernel.External.Verification.Interface.Tten where

import qualified Kernel.External.Verification.Interface.Types as InterfaceTypes
import qualified Kernel.External.Verification.Tten.Flow as TtenFlow
import Kernel.External.Verification.Tten.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

verifyTten ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TtenVerificationCfg ->
  Text ->
  InterfaceTypes.VerifyTtenReq ->
  m ApplicationDetailsResp
verifyTten cfg token req = do
  resp <- TtenFlow.getApplicationDetails cfg token $ makeTtenVerificationReq req
  return resp
  where
    makeTtenVerificationReq :: InterfaceTypes.VerifyTtenReq -> ApplicationDetailsReq
    makeTtenVerificationReq InterfaceTypes.VerifyTtenReq {..} =
      ApplicationDetailsReq
        { tten_no = ttenCertificateNumber
        }
