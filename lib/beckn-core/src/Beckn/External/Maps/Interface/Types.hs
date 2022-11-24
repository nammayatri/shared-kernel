{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.External.Maps.Interface.Types
  ( module Beckn.External.Maps.Interface.Types,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import qualified Beckn.External.Maps.Google.Config as Google
import Beckn.External.Maps.Types
import Beckn.External.Types (Language)
import Beckn.Types.Common
import Beckn.Utils.GenericPretty (PrettyShow)
import Control.Lens.Operators
import Data.Geospatial
import Data.LineString
import Data.OpenApi hiding (components, description)
import qualified Data.OpenApi as OpenApi
import Data.Text
import EulerHS.Prelude

newtype MapsServiceConfig = GoogleConfig Google.GoogleCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TravelMode = CAR | MOTORCYCLE | BICYCLE | FOOT
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data GetDistanceReq a b = GetDistanceReq
  { origin :: a,
    destination :: b,
    travelMode :: Maybe TravelMode
  }
  deriving (Generic, Show)

data GetDistancesReq a b = GetDistancesReq
  { origins :: NonEmpty a,
    destinations :: NonEmpty b,
    travelMode :: Maybe TravelMode
  }
  deriving (Generic, Show)

data GetDistanceResp a b = GetDistanceResp
  { origin :: a,
    destination :: b,
    distance :: Meters,
    duration :: Seconds,
    status :: Text
  }
  deriving (Generic, Show, PrettyShow)

type GetDistancesResp a b = NonEmpty (GetDistanceResp a b)

data GetRoutesReq = GetRoutesReq
  { waypoints :: NonEmpty LatLong,
    mode :: Maybe TravelMode, -- Defaults to CAR
    calcPoints :: Bool -- True (default) if points needs to be calculated
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

type GetRoutesResp = [RouteInfo]

data RouteInfo = RouteInfo
  { duration :: Maybe Seconds,
    distance :: Maybe Meters,
    boundingBox :: Maybe BoundingBoxWithoutCRS,
    snappedWaypoints :: [(LatLong, LatLong)],
    points :: [LatLong]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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

data SnapToRoadReq = SnapToRoadReq
  { points :: [LatLong],
    interpolate :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype SnapToRoadResp = SnapToRoadResp
  { snappedPoints :: [LatLong]
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
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Prediction = Prediction
  { description :: Text,
    placeId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
    location :: LatLong
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
