module Kernel.External.ChallanSearch.Interface
  ( module Kernel.External.ChallanSearch.Interface,
    module Reexport,
  )
where

import Kernel.External.ChallanSearch.Interface.Signzy as CIS
import Kernel.External.ChallanSearch.Interface.Types as Reexport
import Kernel.External.ChallanSearch.Types (ChallanSearchService)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

getPendingChallanCount :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => ChallanSearchServiceConfig -> PendingChallanReq -> m PendingChallanResp
getPendingChallanCount serviceConfig req = case serviceConfig of
  SignzyChallanSearch cfg -> CIS.signzyChallanSearch cfg req

getPendingChallanCountWithFallback ::
  (EncFlow m r, CoreMetrics m, MonadFlow m, Log m, HasRequestId r, MonadReader r m) =>
  (ChallanSearchService -> m ChallanSearchServiceConfig) ->
  [ChallanSearchService] ->
  PendingChallanReq ->
  m PendingChallanResp
getPendingChallanCountWithFallback getServiceConfig providers req = do
  when (null providers) $ throwError $ InternalError "No challan search service provider configured"
  go providers
  where
    go [] = throwError $ InternalError "Not able to fetch challan count with all configured providers"
    go (provider : rest) = do
      logDebug $ "Calling getPendingChallanCount for provider: " <> show provider
      result <- withTryCatch "getPendingChallanCount" $ getServiceConfig provider >>= flip getPendingChallanCount req
      case result of
        Left err -> do
          logError $ "Provider failed: " <> show provider <> ", error: " <> show err
          go rest
        Right res -> pure res
