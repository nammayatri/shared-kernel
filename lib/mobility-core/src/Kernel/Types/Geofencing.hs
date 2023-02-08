module Kernel.Types.Geofencing where

import Kernel.Storage.Esqueleto.Types
import Kernel.Utils.Dhall hiding (maybe)
import Kernel.Utils.GenericPretty
import Database.Esqueleto.Experimental
import EulerHS.Prelude

data GeoRestriction
  = Unrestricted
  | Regions [Text]
  deriving (Show, Generic, FromDhall, FromJSON, ToJSON)

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
  deriving (Show, Generic, FromDhall, PrettyShow, FromJSON, ToJSON)
