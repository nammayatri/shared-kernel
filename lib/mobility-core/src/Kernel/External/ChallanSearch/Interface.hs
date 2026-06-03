module Kernel.External.ChallanSearch.Interface
  ( module Kernel.External.ChallanSearch.Interface,
    module Reexport,
  )
where

import Kernel.External.ChallanSearch.Interface.Signzy as CIS
import Kernel.External.ChallanSearch.Interface.Types as Reexport
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client

getPendingChallanCount :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => ChallanSearchServiceConfig -> PendingChallanReq -> m PendingChallanResp
getPendingChallanCount serviceConfig req = case serviceConfig of
  SignzyChallanSearch cfg -> CIS.signzyChallanSearch cfg req
