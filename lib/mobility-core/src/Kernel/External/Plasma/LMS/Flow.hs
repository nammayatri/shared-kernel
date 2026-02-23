{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Plasma.LMS.Flow where

import EulerHS.Prelude
import Kernel.External.Encryption (decrypt)
import Kernel.External.Plasma.LMS.Client
import Kernel.External.Plasma.LMS.Config
import Kernel.External.Plasma.LMS.Types
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

-- | Get LMS modules for a driver
-- The API key is retrieved from the config table using the config key specified in LMSCfg
getLMSModules ::
  ( CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  LMSCfg ->
  Text -> -- Driver ID
  m LMSModulesResp
getLMSModules cfg driverId = do
  apiKey <- decrypt cfg.apiKey
  withLogTag "getLMSModules" $ getModules cfg apiKey driverId
