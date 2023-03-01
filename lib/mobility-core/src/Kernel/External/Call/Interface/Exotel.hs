{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Call.Interface.Exotel where

import EulerHS.Prelude
import qualified Kernel.External.Call.Exotel.Client as Exotel
import Kernel.External.Call.Exotel.Config (ExotelCfg)
import Kernel.External.Call.Exotel.Types as Exotel
import Kernel.External.Call.Interface.Types as Interface
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

initiateCall ::
  ( CoreMetrics m,
    MonadFlow m,
    ToJSON a
  ) =>
  ExotelCfg ->
  InitiateCallReq a ->
  m InitiateCallResp
initiateCall config InitiateCallReq {..} = do
  res <-
    Exotel.initiateCall config $
      ExotelInitiateCallReq
        { from = fromPhoneNum,
          to = toPhoneNum,
          callerId = config.callerId,
          statusCallbackUrl = config.callbackUrl,
          customField = getAttachments attachments
        }
  return $
    InitiateCallResp
      { callId = res.exoCall.exoSid.getExotelCallSID,
        callStatus = exotelStatusToInterfaceStatus res.exoCall.exoStatus
      }

exotelStatusToInterfaceStatus :: Exotel.ExotelCallStatus -> Interface.CallStatus
exotelStatusToInterfaceStatus = \case
  Exotel.QUEUED -> Interface.QUEUED
  Exotel.RINGING -> Interface.RINGING
  Exotel.IN_PROGRESS -> Interface.IN_PROGRESS
  Exotel.COMPLETED -> Interface.COMPLETED
  Exotel.FAILED -> Interface.FAILED
  Exotel.BUSY -> Interface.BUSY
  Exotel.NO_ANSWER -> Interface.NO_ANSWER
  Exotel.CANCELED -> Interface.CANCELED
  Exotel.INVALID_STATUS -> Interface.INVALID_STATUS
