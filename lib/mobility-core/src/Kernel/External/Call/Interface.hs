{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Call.Interface
  ( module Reexport,
    module Kernel.External.Call.Interface,
  )
where

import EulerHS.Prelude
import Kernel.External.Call.Exotel.Config as Reexport
import qualified Kernel.External.Call.Interface.Exotel as Exotel
import Kernel.External.Call.Interface.Types as Reexport
import Kernel.External.Call.Types as Reexport
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common

initiateCall ::
  ( CoreMetrics m,
    EncFlow m r,
    ToJSON a
  ) =>
  CallServiceConfig ->
  InitiateCallReq a ->
  m InitiateCallResp
initiateCall config req = do
  case config of
    ExotelConfig ec -> Exotel.initiateCall ec req
