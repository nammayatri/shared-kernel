module Kernel.External.ChallanSearch.Interface.Signzy (signzyChallanSearch) where

import qualified Data.Text as T
import Kernel.External.ChallanSearch.Interface.Types as CIT
import Kernel.External.ChallanSearch.Signzy.API as CSA
import Kernel.External.ChallanSearch.Signzy.Config as CSC
import Kernel.External.ChallanSearch.Signzy.Types as CST
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Logging (logInfo)
import Kernel.Utils.Servant.Client

signzyChallanSearch :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => CSC.SignzyChallanSearchCfg -> CIT.PendingChallanReq -> m CIT.PendingChallanResp
signzyChallanSearch cfg req = do
  apiKey <- decrypt cfg.apiKey
  let signzyReq = CST.ChallanSearchReq {vehicleNumber = req.vehicleNumber}
  signzyResp <- CSA.challanSearch cfg.url apiKey signzyReq
  let pendingChallans = filter isPending signzyResp.result.challanDetails
  logInfo $ "Signzy challan search for " <> req.vehicleNumber <> ": found " <> show (length pendingChallans) <> " pending out of " <> show (length signzyResp.result.challanDetails) <> " total challans"
  pure $ CIT.PendingChallanResp {pendingChallanCount = length pendingChallans}
  where
    isPending :: CST.ChallanDetail -> Bool
    isPending detail = T.toLower detail.challanStatus == "pending"
