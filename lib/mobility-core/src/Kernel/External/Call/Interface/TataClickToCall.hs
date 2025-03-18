{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Call.Interface.TataClickToCall where

import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import Kernel.External.Call.Interface.Types as Interface
import qualified Kernel.External.Call.TataClickToCall.Client as TataClickToCall
import Kernel.External.Call.TataClickToCall.Config
import Kernel.External.Call.TataClickToCall.Types as TataClickToCall
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Logging (logDebug)

initiateCall ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    ToJSON a
  ) =>
  TataClickToCallCfg ->
  InitiateCallReq a ->
  m InitiateCallResp
initiateCall config InitiateCallReq {..} = do
  logDebug $ "URL for TataClickToCall: " <> show config.url
  let tataClickToCallReq = getTataClickToCallReq
  res <-
    TataClickToCall.tataInitiateCall config tataClickToCallReq
  return $
    InitiateCallResp
      { callId = fromMaybe T.empty (getField @"call_id" res),
        callStatus = Interface.QUEUED
      }
  where
    getTataClickToCallReq =
      ClickToCallRequest
        { agent_number = fromPhoneNum,
          destination_number = fromMaybe T.empty toPhoneNum
        }
