{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SOS.Interface
  ( sendInitialSOS,
    sendSOSTrace,
    updateSOSStatus,
    uploadMedia,
    module Reexport,
  )
where

import Kernel.External.Encryption
import qualified Kernel.External.SOS.Interface.ERSS as ERSS
import qualified Kernel.External.SOS.Interface.GJ112 as GJ112
import Kernel.External.SOS.Interface.Types as Reexport
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

-- | Send Initial SOS Signal - dispatches to appropriate provider
sendInitialSOS ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SOSServiceConfig ->
  InitialSOSReq ->
  m InitialSOSRes
sendInitialSOS config req = case config of
  ERSSConfig erssCfg -> ERSS.sendInitialSOS erssCfg req
  GJ112Config gj112Cfg -> GJ112.sendInitialSOS gj112Cfg req

-- | Send SOS Trace (location update) - dispatches to appropriate provider
sendSOSTrace ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SOSServiceConfig ->
  SOSTraceReq ->
  m SOSTraceRes
sendSOSTrace config req = case config of
  ERSSConfig erssCfg -> ERSS.sendSOSTrace erssCfg req
  GJ112Config gj112Cfg -> GJ112.sendSOSTrace gj112Cfg req

-- | Update SOS Status - dispatches to appropriate provider
updateSOSStatus ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SOSServiceConfig ->
  SOSStatusUpdateReq ->
  m SOSStatusUpdateRes
updateSOSStatus config req = case config of
  ERSSConfig erssCfg -> ERSS.updateSOSStatus erssCfg req
  GJ112Config gj112Cfg -> GJ112.updateSOSStatus gj112Cfg req

-- | Upload Media File - dispatches to appropriate provider
uploadMedia ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SOSServiceConfig ->
  Text ->
  Text ->
  FilePath ->
  m SOSMediaUploadRes
uploadMedia config phoneNumber fileName filePath = case config of
  ERSSConfig erssCfg -> ERSS.uploadMedia erssCfg phoneNumber fileName filePath
  GJ112Config _gj112Cfg -> pure $ SOSMediaUploadRes False (Just "Media upload not implemented for GJ112")
