{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.BackgroundVerification.Interface.Checkr (module Reexport, module Kernel.External.BackgroundVerification.Interface.Checkr) where

import Kernel.External.BackgroundVerification.Checkr.Config
import Kernel.External.BackgroundVerification.Checkr.Flow as Reexport
import Kernel.External.BackgroundVerification.Interface.Types
import Kernel.External.Encryption
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common

createInvitation ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r
  ) =>
  CheckrCfg ->
  CreateInvitationReqI ->
  m CreateInvitationResp
createInvitation cfg CreateInvitationReqI {..} = createCheckrInvitation cfg CreateInvitationReq {package = cfg.package, ..}
