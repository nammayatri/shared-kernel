{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.Interface.Trinity
  ( sendInitialSOS,
    sendSOSTrace,
    updateSOSStatus,
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.SOS.Interface.Types as Interface
import Kernel.External.SOS.Trinity.Config
import qualified Kernel.External.SOS.Trinity.Flow as TRFlow
import qualified Kernel.External.SOS.Trinity.Types as TR
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

-- | Send Initial SOS - converts interface types to Trinity types and delegates
sendInitialSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TrinityCfg ->
  Interface.InitialSOSReq ->
  m Interface.InitialSOSRes
sendInitialSOS config req = do
  let trReq = toTrinitySOSReq config req
  trRes <- TRFlow.sendSOS config trReq
  pure $ fromTrinitySOSRes trRes

-- | Send SOS Trace - Trinity does not support trace, return success
sendSOSTrace ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TrinityCfg ->
  Interface.SOSTraceReq ->
  m Interface.SOSTraceRes
sendSOSTrace _config _req =
  pure $
    Interface.SOSTraceRes
      { success = True,
        errorMessage = Nothing
      }

-- | Update SOS Status - Trinity does not support status update, return success
updateSOSStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  TrinityCfg ->
  Interface.SOSStatusUpdateReq ->
  m Interface.SOSStatusUpdateRes
updateSOSStatus _config _req =
  pure $
    Interface.SOSStatusUpdateRes
      { success = True,
        errorMessage = Nothing
      }

-- Conversion helpers

toTrinitySOSReq :: TrinityCfg -> Interface.InitialSOSReq -> TR.TrinitySOSReq
toTrinitySOSReq config Interface.InitialSOSReq {..} =
  TR.TrinitySOSReq
    { clientId = Just config.clientId,
      clientCode = Just config.clientCode,
      name = fromMaybe "" senderName,
      city = city,
      address = address,
      deviceUuid = imeiNo,
      email = email,
      relativeName1 = emergencyContact1Name,
      relativeName2 = emergencyContact2Name,
      relativeContact1 = emergencyContact1Phone,
      relativeContact2 = emergencyContact2Phone,
      gender = gender,
      simNo = mobileNo,
      datetime = Just dateTime,
      emergencyMessage = emergencyMessage,
      latitude = show latitude,
      longitude = show longitude,
      videoPath = videoPath,
      driverName = driverName,
      driverContactNo = driverContactNo,
      vehicleNo = vehicleNo,
      vehicleModel = vehicleModel,
      vehLat = maybe (show latitude) show vehicleLat,
      vehLng = maybe (show longitude) show vehicleLon,
      deviceType = config.deviceType,
      vehLocUrl = vehicleLocationUrl
    }

fromTrinitySOSRes :: TR.TrinitySOSRes -> Interface.InitialSOSRes
fromTrinitySOSRes res =
  Interface.InitialSOSRes
    { success = res.status,
      trackingId = res.caseId,
      errorMessage = if res.status then Nothing else res.message
    }
