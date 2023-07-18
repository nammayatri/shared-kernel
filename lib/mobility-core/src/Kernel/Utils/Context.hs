{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
  Cab.City ->
  Cab.Country ->
  Bool ->
  m Cab.Context
buildTaxiContext action msgId txnId bapId bapUri bppId bppUri city country autoAssignEnabled = do
  currTime <- getCurrentTime
  let max_callbacks = if autoAssignEnabled && action == Cab.SELECT then Just 1 else Nothing
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
        country,
        city,
        max_callbacks
      }
