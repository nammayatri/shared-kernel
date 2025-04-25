{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Call.Types
  ( module Kernel.External.Call.Types,
  )
where

import Data.OpenApi
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Storage.Esqueleto (derivePersistField)

data CallService = Exotel | Knowlarity | TwillioCall | TataClickToCall
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''CallService)

derivePersistField "CallService"
