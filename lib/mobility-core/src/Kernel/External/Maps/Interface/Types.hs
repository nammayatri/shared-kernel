{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.Maps.Interface.Types
  ( module Kernel.External.Maps.Interface.Types,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Control.Lens.Operators
import Data.Geospatial
import Data.LineString
import Data.OpenApi hiding (components, description)
import qualified Data.OpenApi as OpenApi
import Data.Text
import Deriving.Aeson
import EulerHS.Prelude
import qualified Kernel.External.Maps.Google.Config as Google
import qualified Kernel.External.Maps.MMI.Config as MMI
import qualified Kernel.External.Maps.OSRM.Config as OSRM
import Kernel.External.Maps.Types
import Kernel.External.Types (Language)
import Kernel.Types.Common
import Kernel.Utils.GenericPretty (PrettyShow)

data MapsServiceConfig = GoogleConfig Google.GoogleCfg | OSRMConfig OSRM.OSRMCfg | MMIConfig MMI.MMICfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] MapsServiceConfig

data TravelMode = CAR | MOTORCYCLE | BICYCLE | FOOT
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data GetDistanceReq a b = GetDistanceReq
  { origin :: a,
    destination :: b,
    travelMode :: Maybe TravelMode
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance (ToSchema a, ToSchema b) => ToSchema (GetDistanceReq a b)

data GetDistancesReq a b = GetDistancesReq
  { origins :: NonEmpty a,
    destinations :: NonEmpty b,
    travelMode :: Maybe TravelMode
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

data GetDistanceResp a b = GetDistanceResp
  { origin :: a,
    destination :: b,
    distance :: Meters,
    duration :: Seconds,
    status :: Text
  }
  deriving (Generic, Show, PrettyShow, FromJSON, ToJSON)

instance (ToSchema a, ToSchema b) => ToSchema (GetDistanceResp a b)

type GetDistancesResp a b = NonEmpty (GetDistanceResp a b)

data GetDistanceData a b = GetDistanceData
  { request :: GetDistancesReq a b,
    response :: [GetDistanceResp a b]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data GetRoutesReq = GetRoutesReq
  { waypoints :: NonEmpty LatLong,
    mode :: Maybe TravelMode, -- Defaults to CAR
    calcPoints :: Bool -- True (default) if points needs to be calculated
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data GetRoutesReqProxy = GetRoutesReqProxy
  { waypoints :: [LatLong],
    origin :: LatLong,
    destination :: LatLong,
    mode :: Maybe TravelMode, -- Defaults to CAR
    calcPoints :: Bool -- True (default) if points needs to be calculated
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

type GetRoutesResp = [RouteInfo]

data RouteInfo = RouteInfo
  { duration :: Maybe Seconds,
    distance :: Maybe Meters,
    boundingBox :: Maybe BoundingBoxWithoutCRS,
    snappedWaypoints :: [LatLong],
    points :: [LatLong]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

instance ToSchema BoundingBoxWithoutCRS where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Value)
    return $
      NamedSchema (Just "BoundingBoxWithoutCRS") $
        aSchema
          & OpenApi.description
            ?~ "https://datatracker.ietf.org/doc/html/rfc7946#section-5"

instance ToSchema GeospatialGeometry where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Value)
    return $
      NamedSchema (Just "GeospatialGeometry") $
        aSchema
          & OpenApi.description
            ?~ "https://datatracker.ietf.org/doc/html/rfc7946#section-2"

newtype SnapToRoadReq = SnapToRoadReq
  { points :: [LatLong]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SnapToRoadResp = SnapToRoadResp
  { distance :: HighPrecMeters,
    snappedPoints :: [LatLong]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutoCompleteReq = AutoCompleteReq
  { input :: Text,
    sessionToken :: Maybe Text,
    location :: Text,
    radius :: Integer,
    language :: Language
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype AutoCompleteResp = AutoCompleteResp
  { predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data Prediction = Prediction
  { description :: Text,
    placeId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data GetPlaceDetailsReq = GetPlaceDetailsReq
  { placeId :: Text,
    sessionToken :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype GetPlaceDetailsResp = GetPlaceDetailsResp
  { location :: LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetPlaceNameBy = ByLatLong LatLong | ByPlaceId Text
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data GetPlaceNameReq = GetPlaceNameReq
  { getBy :: GetPlaceNameBy,
    sessionToken :: Maybe Text,
    language :: Maybe Language
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type GetPlaceNameResp = [PlaceName]

data PlaceName = PlaceName
  { formattedAddress :: Maybe Text,
    addressComponents :: [AddressResp],
    plusCode :: Maybe Text,
    location :: LatLong,
    placeId :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddressResp = AddressResp
  { longName :: Text,
    shortName :: Text,
    types :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
