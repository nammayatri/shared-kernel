{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.Interface.GJ112
  ( sendInitialSOS,
    sendSOSTrace,
    updateSOSStatus,
  )
where

import Kernel.External.Encryption
import Kernel.External.SOS.GJ112.Config
import qualified Kernel.External.SOS.GJ112.Flow as GJ112Flow
import qualified Kernel.External.SOS.GJ112.Types as GJ112
import qualified Kernel.External.SOS.Interface.Types as Interface
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

-- | Send Initial SOS - converts interface types to GJ112 types and delegates
sendInitialSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GJ112Cfg ->
  Interface.InitialSOSReq ->
  m Interface.InitialSOSRes
sendInitialSOS config req = do
  let gj112Req = toGJ112SOSReq config req
  gj112Res <- GJ112Flow.sendSOS config gj112Req
  pure $ fromGJ112SOSRes gj112Res

-- | Send SOS Trace - GJ112 does not support trace, return success
sendSOSTrace ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GJ112Cfg ->
  Interface.SOSTraceReq ->
  m Interface.SOSTraceRes
sendSOSTrace _config _req =
  pure $
    Interface.SOSTraceRes
      { success = True,
        errorMessage = Nothing
      }

-- | Update SOS Status - GJ112 does not support status update, return success
updateSOSStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  GJ112Cfg ->
  Interface.SOSStatusUpdateReq ->
  m Interface.SOSStatusUpdateRes
updateSOSStatus _config _req =
  pure $
    Interface.SOSStatusUpdateRes
      { success = True,
        errorMessage = Nothing
      }

-- Conversion helpers

toGJ112SOSReq :: GJ112Cfg -> Interface.InitialSOSReq -> GJ112.GJ112SOSReq
toGJ112SOSReq config Interface.InitialSOSReq {..} =
  GJ112.GJ112SOSReq
    { clientId = config.clientId,
      clientCode = config.clientCode,
      name = fromMaybe "" senderName,
      city = fromMaybe "" city,
      address = address,
      deviceUuid = imeiNo,
      email = email,
      relativeName1 = emergencyContact1Name,
      relativeName2 = emergencyContact2Name,
      relativeContact1 = emergencyContact1Phone,
      relativeContact2 = emergencyContact2Phone,
      gender = gender,
      simNo = Just mobileNo,
      datetime = dateTime,
      emergencyMessage = fromMaybe "SOS Emergency" emergencyMessage,
      latitude = show latitude,
      longitude = show longitude,
      videoPath = videoPath,
      driverName = driverName,
      driverContactNo = driverContactNo,
      vehicleNo = vehicleNo,
      vehicleModel = vehicleModel,
      vehLat = show <$> vehicleLat,
      vehLng = show <$> vehicleLon,
      deviceType = deviceType,
      vehLocUrl = vehicleLocationUrl,
      vendorName = vendorName,
      vehicleColor = vehicleColor,
      vehicleType = vehicleType,
      vehicleMake = vehicleMake,
      vehicleAppearanceNotes = vehicleAppearanceNotes
    }

fromGJ112SOSRes :: GJ112.GJ112SOSRes -> Interface.InitialSOSRes
fromGJ112SOSRes res =
  Interface.InitialSOSRes
    { success = maybe False (== 200) res.responseCode,
      trackingId = show <$> res.referenceId,
      errorMessage = if maybe False (== 200) res.responseCode then Nothing else res.message
    }
