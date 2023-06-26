{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Geofencing where

import Database.Esqueleto.Experimental
import EulerHS.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Utils.Dhall hiding (maybe)
import Kernel.Utils.GenericPretty

data GeoRestriction
  = Unrestricted
  | Regions [Text]
  deriving (Show, Generic, FromDhall, FromJSON, ToJSON, Read)

instance PrettyShow GeoRestriction where
  prettyShow = prettyShow . geoRestrictionToMaybeList

geoRestrictionToMaybeList :: GeoRestriction -> Maybe [Text]
geoRestrictionToMaybeList Unrestricted = Nothing
geoRestrictionToMaybeList (Regions xs) = Just xs

maybeListToGeoRestriction :: Maybe [Text] -> GeoRestriction
maybeListToGeoRestriction = maybe Unrestricted Regions

instance PersistField GeoRestriction where
  toPersistValue = toPersistValue . fmap PostgresList . geoRestrictionToMaybeList
  fromPersistValue x = maybeListToGeoRestriction <$> fromPersistValue x

instance PersistFieldSql GeoRestriction where
  sqlType _ = SqlString

data GeofencingConfig = GeofencingConfig
  { origin :: GeoRestriction,
    destination :: GeoRestriction
  }
  deriving (Show, Generic, FromDhall, PrettyShow, FromJSON, ToJSON, Read)
