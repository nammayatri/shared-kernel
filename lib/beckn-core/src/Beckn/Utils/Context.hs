module Beckn.Utils.Context where

import Beckn.Types.App
import qualified Beckn.Types.Core.Context as Cab
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import EulerHS.Prelude

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
        timestamp = currTime,
        country = "IND",
        city = "Kochi"
      }
