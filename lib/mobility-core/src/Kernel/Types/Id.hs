 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Types.Id where

import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import Dhall
import EulerHS.Prelude
import Kernel.Types.GuidLike
import Kernel.Types.MonadGuid
import Kernel.Utils.Example (Example (..), idExample)
import Kernel.Utils.GenericPretty
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData)

newtype Id domain = Id
  {getId :: Text}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, ToSchema, ToParamSchema, FromField, PrettyShow)

cast :: Id a -> Id b
cast = Id . getId

instance Example (Id a) where
  example = Id idExample

instance IsString (Id d) where
  fromString = Id . Text.pack

instance FromHttpApiData (Id a) where
  parseUrlPiece = pure . Id

instance (MonadGuid m) => GuidLike m (Id a) where
  generateGUID = Id <$> generateGUIDText

newtype ShortId domain = ShortId
  { getShortId :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, ToSchema, ToParamSchema, PrettyShow)

instance FromDhall (ShortId a) where
  autoWith _ = ShortId <$> strictText

instance IsString (ShortId d) where
  fromString = ShortId . Text.pack

instance FromHttpApiData (ShortId a) where
  parseUrlPiece = pure . ShortId

instance (MonadGuid m) => GuidLike m (ShortId a) where
  generateGUID = ShortId <$> generateGUIDText
