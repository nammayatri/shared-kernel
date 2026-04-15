{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Moengage.Interface
  ( pushEvent,
    module Reexport,
  )
where

import Kernel.External.Encryption
import Kernel.External.Moengage.Interface.Types as Reexport
import qualified Kernel.External.Moengage.Moengage.Flow as MoengageFlow
import Kernel.External.Moengage.Moengage.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common (HasRequestId)

-- | Push event to Moengage - dispatches to appropriate provider
pushEvent ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  MoengageServiceConfig ->
  MoengageEventReq ->
  m (Maybe MoengageEventResp)
pushEvent config req = case config of
  MoengageConfig moengageCfg -> MoengageFlow.pushEvent moengageCfg req
