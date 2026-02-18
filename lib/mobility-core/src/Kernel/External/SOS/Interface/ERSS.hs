{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.Interface.ERSS
  ( sendInitialSOS,
    sendSOSTrace,
    updateSOSStatus,
    uploadMedia,
  )
where

import Kernel.External.Encryption
import Kernel.External.SOS.ERSS.Config
import qualified Kernel.External.SOS.ERSS.Flow as EF
import qualified Kernel.External.SOS.ERSS.Types as ERSS
import qualified Kernel.External.SOS.Interface.Types as Interface
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

-- | Send Initial SOS - converts interface types to ERSS types and delegates
sendInitialSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Interface.InitialSOSReq ->
  m Interface.InitialSOSRes
sendInitialSOS config req = do
  let erssReq = toERSSInitialReq config req
  erssRes <- EF.sendInitialSOS config erssReq
  pure $ fromERSSInitialRes erssRes

-- | Send SOS Trace - converts interface types to ERSS types and delegates
sendSOSTrace ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Interface.SOSTraceReq ->
  m Interface.SOSTraceRes
sendSOSTrace config req = do
  let erssReq = toERSSTraceReq config req
  erssRes <- EF.sendSOSTrace config erssReq
  pure $ fromERSSTraceRes erssRes

-- | Update SOS Status - acknowledges an inbound C-DAC status update webhook.
-- Section 7 of the ERSS doc is an inbound webhook: C-DAC pushes status updates
-- to our endpoint. This function processes the update and returns acknowledgment.
updateSOSStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Interface.SOSStatusUpdateReq ->
  m Interface.SOSStatusUpdateRes
updateSOSStatus _config _req =
  pure $
    Interface.SOSStatusUpdateRes
      { success = True,
        errorMessage = Nothing
      }

-- Conversion helpers

toERSSInitialReq :: ERSSCfg -> Interface.InitialSOSReq -> ERSS.ERSSInitialSOSReq
toERSSInitialReq config Interface.InitialSOSReq {..} =
  ERSS.ERSSInitialSOSReq
    { id = sosId,
      dateTime = dateTime,
      latitude = latitude,
      longitude = longitude,
      speed = speed,
      mobileNo = mobileNo,
      imeiNo = imeiNo,
      gpsProvider = gpsProvider,
      senderName = senderName,
      address = address,
      gpsAccuracy = gpsAccuracy,
      stateCode = stateCode,
      silentCommunication = fmap (\b -> if b then "true" else "false") silentCommunication,
      specialNeeds = specialNeeds,
      dob = dob,
      gender = gender,
      attachmentFileName = attachmentFileName,
      authId = config.authId,
      authCode = config.authCode
    }

fromERSSInitialRes :: ERSS.ERSSInitialSOSRes -> Interface.InitialSOSRes
fromERSSInitialRes ERSS.ERSSApiResponse {..} =
  Interface.InitialSOSRes
    { success = resultCode == "OPERATION_SUCCESS",
      trackingId = fmap (\p -> show p.signalId) payLoad,
      errorMessage = errorMsg
    }

-- | Build ERSS Trace PACKET string.
-- Format: "dateTime,latitude,longitude,mobileNo,authId,authCode,SOS#"
buildTracePacket :: Text -> Double -> Double -> Text -> Text -> Text -> Text
buildTracePacket dt lat lon mobile authId authCode =
  dt
    <> ","
    <> show lat
    <> ","
    <> show lon
    <> ","
    <> mobile
    <> ","
    <> authId
    <> ","
    <> authCode
    <> ",SOS#"

toERSSTraceReq :: ERSSCfg -> Interface.SOSTraceReq -> ERSS.ERSSTraceReq
toERSSTraceReq config Interface.SOSTraceReq {..} =
  ERSS.ERSSTraceReq
    { packet = buildTracePacket dateTime latitude longitude mobileNo config.authId config.authCode
    }

fromERSSTraceRes :: ERSS.ERSSTraceRes -> Interface.SOSTraceRes
fromERSSTraceRes ERSS.ERSSApiResponse {..} =
  Interface.SOSTraceRes
    { success = resultCode == "OPERATION_SUCCESS",
      errorMessage = errorMsg
    }

-- | Upload Media
uploadMedia ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ERSSCfg ->
  Text ->
  Text ->
  FilePath ->
  m Interface.SOSMediaUploadRes
uploadMedia config phoneNumber fileName filePath = do
  erssRes <- EF.uploadMedia config phoneNumber fileName filePath
  pure $ fromERSSMediaUploadRes erssRes

fromERSSMediaUploadRes :: ERSS.ERSSMediaUploadRes -> Interface.SOSMediaUploadRes
fromERSSMediaUploadRes ERSS.ERSSApiResponse {..} =
  Interface.SOSMediaUploadRes
    { success = resultCode == "OPERATION_SUCCESS",
      errorMessage = errorMsg
    }
