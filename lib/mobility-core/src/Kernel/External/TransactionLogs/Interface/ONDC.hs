{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.TransactionLogs.Interface.ONDC where

import qualified Kernel.External.TransactionLogs.Interface.Types as IT
import qualified Kernel.External.TransactionLogs.ONDC.Flow as OF
import Kernel.External.TransactionLogs.ONDC.Types as ONDC
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common

pushTxnLogs ::
  (CoreMetrics m, MonadFlow m) =>
  ONDCConfig ->
  IT.TransactionLogReq ->
  m ONDC.ONDCResponse
pushTxnLogs config req =
  OF.pushTxnLogsAPI config (mkONDCRequest req)

mkONDCRequest :: IT.TransactionLogReq -> ONDC.ONDCRequest
mkONDCRequest IT.TransactionLogReq {..} =
  ONDC.ONDCRequest
    { _type = logType,
      _data = logData
    }
