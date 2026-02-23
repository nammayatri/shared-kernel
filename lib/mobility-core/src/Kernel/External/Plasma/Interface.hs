{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Kernel.External.Plasma.Interface
  ( module Reexport,
    getLMSModules,
  )
where

import EulerHS.Prelude
import Kernel.External.Plasma.Interface.Types as Reexport
import Kernel.External.Plasma.LMS.Config as Reexport
import qualified Kernel.External.Plasma.LMS.Flow as LMSFlow
import Kernel.External.Plasma.LMS.Types as Reexport
import Kernel.External.Plasma.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common

-- | Get LMS modules for a driver
-- This is the main interface function exposed to NY side
-- Takes PlasmaServiceConfig and driverId, then calls the appropriate service function
getLMSModules ::
  ( CoreMetrics m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  PlasmaServiceConfig ->
  Text -> -- Driver ID
  m LMSModulesResp
getLMSModules serviceConfig driverId = do
  case serviceConfig of
    LMSConfig cfg -> LMSFlow.getLMSModules cfg driverId
