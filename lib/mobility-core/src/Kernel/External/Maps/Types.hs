{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Maps.Types
  ( module Kernel.External.Maps.Types,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Control.Lens.Operators
import Data.Geospatial
import Data.LineString
import Data.OpenApi
import Data.Text
import Database.Beam.Backend
import qualified Database.Beam.Backend.SQL.AST as B
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList, mkBeamInstancesForList)
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Utils.GenericPretty (PrettyShow)
import Data.Singletons.TH
import Data.Text hiding (toLower)
import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified GHC.Show as Show
import Kernel.Prelude
import Kernel.Storage.Esqueleto (PersistField, PersistFieldSql, derivePersistField)
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

data MapsService = Google | OSRM | MMI | NextBillion
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable MapsService

instance FromField MapsService where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MapsService where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MapsService

instance FromBackendRow Postgres MapsService

$(mkBeamInstancesForEnumAndList ''MapsService)

availableMapsServices :: [MapsService]
availableMapsServices = [Google, OSRM, MMI, NextBillion]

derivePersistField "MapsService"

data LatLong = LatLong
  { lat :: Double,
    lon :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema, PrettyShow, Ord, Read)

instance HasSqlValueSyntax B.Value LatLong where
  sqlValueSyntax = autoSqlValueSyntax

$(mkBeamInstancesForList ''LatLong)

instance ToParamSchema LatLong where
  toParamSchema _ =
    mempty
      & title ?~ "LatLong"
      & type_ ?~ OpenApiString
      & format ?~ "<latitude>,<longitude>"

instance FromHttpApiData LatLong where
  parseUrlPiece a = do
    (lat, long) <- case splitOn "," a of
      [lat, long] -> Right (lat, long)
      _ -> Left "Unable to parse LatLong,"
    LatLong <$> readEither lat <*> readEither long

instance ToHttpApiData LatLong where
  toQueryParam :: LatLong -> Text
  toQueryParam LatLong {..} = show lat <> "," <> show lon

-- sum should be always 100
data MapsServiceUsage (msum :: MapsServiceUsageMethod) = MapsServiceUsage
  { mapsService :: SMapsService msum,
    usePercentage :: Bool, -- False by default
    googlePercentage :: Maybe Int,
    osrmPercentage :: Maybe Int,
    mmiPercentage :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data MapsServiceUsagePercentage (msum :: MapsServiceUsageMethod) = MapsServiceUsagePercentage
  { usePercentage :: Bool, -- False by default
    googlePercentage :: Maybe Int,
    osrmPercentage :: Maybe Int,
    mmiPercentage :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON)

mkMapsServiceUsage :: SMapsService msum -> MapsServiceUsagePercentage msum -> MapsServiceUsage msum
mkMapsServiceUsage mapsService MapsServiceUsagePercentage {..} = MapsServiceUsage {..}

mkMapsServiceUsagePercentage :: MapsServiceUsage msum -> MapsServiceUsagePercentage msum
mkMapsServiceUsagePercentage MapsServiceUsage {..} = MapsServiceUsagePercentage {..}

-- strict maps service for more type safety

newtype SMapsService (msum :: MapsServiceUsageMethod) = SMapsService
  { getStrictMapsService :: MapsService
  }
  deriving newtype (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, PrettyShow, PersistField, PersistFieldSql, FromField)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be (SMapsService msum) where
  sqlValueSyntax = sqlValueSyntax . getStrictMapsService

instance BeamSqlBackend be => B.HasSqlEqualityCheck be (SMapsService msum)

instance Typeable msum => FromBackendRow Postgres (SMapsService msum)

data MapsServiceUsageMethod
  = GetDistances
  | GetEstimatedPickupDistances
  | GetRoutes
  | GetPickupRoutes
  | GetTripRoutes
  | SnapToRoad
  | GetPlaceName
  | GetPlaceDetails
  | AutoComplete
  | GetDistancesForCancelRide

genSingletons [''MapsServiceUsageMethod]

-- TODO add some generic instance
instance Show MapsServiceUsageMethod where
  show = \case
    GetDistances -> "getDistances"
    GetEstimatedPickupDistances -> "getEstimatedPickupDistances"
    GetRoutes -> "getRoutes"
    GetPickupRoutes -> "getPickupRoutes"
    GetTripRoutes -> "getTripRoutes"
    SnapToRoad -> "snapToRoad"
    GetPlaceName -> "getPlaceName"
    GetPlaceDetails -> "getPlaceDetails"
    AutoComplete -> "autoComplete"
    GetDistancesForCancelRide -> "getDistancesForCancelRide"
