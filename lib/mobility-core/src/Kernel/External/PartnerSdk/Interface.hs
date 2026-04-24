module Kernel.External.PartnerSdk.Interface
  ( module Reexport,
    module Kernel.External.PartnerSdk.Interface,
  )
where

import qualified Kernel.External.PartnerSdk.Interface.Aarokya as Aarokya
import Kernel.External.PartnerSdk.Interface.Types
import Kernel.External.PartnerSdk.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

generateToken ::
  ( EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  PartnerSdkConfig ->
  GenerateTokenReq ->
  m GenerateTokenResp
generateToken serviceConfig req = case serviceConfig of
  AarokyaPartnerSdkConfig cfg -> Aarokya.generateToken cfg req
