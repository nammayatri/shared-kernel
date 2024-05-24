{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.Verification.GovtData.Storage.Query where

import Kernel.Beam.Functions (createWithKV, findAllWithOptionsKV)
import Kernel.Beam.Lib.Utils
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Verification.GovtData.Storage.Beam as BeamGRC
import qualified Kernel.External.Verification.GovtData.Types as GD
import Kernel.Prelude
import qualified Sequelize as Se

create :: (HasSchemaName BeamGRC.GovtDataRCT, KvDbFlow m r) => GD.GovtDataResponse -> m ()
create = createWithKV

findByRCNumber :: (HasSchemaName BeamGRC.GovtDataRCT, KvDbFlow m r) => Text -> m (Maybe GD.GovtDataResponse)
findByRCNumber rcNumber = findAllWithOptionsKV [Se.Is BeamGRC.registrationNumber $ Se.Eq (Just rcNumber)] (Se.Desc BeamGRC.createdAt) (Just 1) Nothing <&> listToMaybe

instance FromTType' BeamGRC.GovtDataRC GD.GovtDataResponse where
  fromTType' BeamGRC.GovtDataRCT {..} = do
    pure $
      Just
        GD.GovtDataResponse
          { ..
          }

instance ToTType' BeamGRC.GovtDataRC GD.GovtDataResponse where
  toTType' GD.GovtDataResponse {..} = do
    BeamGRC.GovtDataRCT
      { ..
      }
