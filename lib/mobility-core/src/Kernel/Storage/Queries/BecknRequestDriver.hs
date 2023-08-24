{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Queries.BecknRequestDriver where

-- import Kernel.Storage.Esqueleto as Esq

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV)
import Kernel.Prelude
import qualified Kernel.Storage.Beam.BecknRequestDriver as BeamBR
import qualified Kernel.Types.BecknRequest as Domain
import Kernel.Types.Id
import Kernel.Utils.Common

logBecknRequest :: MonadFlow m => Text -> Text -> m ()
logBecknRequest reqJSON sign = do
  uuid <- generateGUID
  now <- getCurrentTime
  let request =
        Domain.BecknRequest
          { id = uuid,
            timeStamp = now,
            becknRequest = reqJSON,
            signatureHeader = sign
          }
  createWithKV request

instance FromTType' BeamBR.BecknRequest Domain.BecknRequest where
  fromTType' BeamBR.BecknRequestT {..} = do
    pure $
      Just
        Domain.BecknRequest
          { id = Id id,
            ..
          }

instance ToTType' BeamBR.BecknRequest Domain.BecknRequest where
  toTType' Domain.BecknRequest {..} = do
    BeamBR.BecknRequestT
      { BeamBR.id = getId id,
        ..
      }
