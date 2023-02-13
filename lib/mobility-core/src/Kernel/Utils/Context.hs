module Kernel.Utils.Context where

import EulerHS.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Cab
import Kernel.Types.MonadGuid
import Kernel.Types.Time
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

buildTaxiContext ::
  (MonadTime m, MonadGuid m) =>
  Cab.Action ->
  Text ->
  Maybe Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  m Cab.Context
buildTaxiContext action msgId txnId bapId bapUri bppId bppUri = do
  currTime <- getCurrentTime
  return $
    Cab.Context
      { domain = Cab.MOBILITY,
        action,
        core_version = "0.9.3",
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = bppId,
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = msgId,
        timestamp = UTCTimeRFC3339 currTime,
        country = "IND",
        city = "Kochi"
      }
