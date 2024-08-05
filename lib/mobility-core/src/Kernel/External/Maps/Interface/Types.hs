{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.External.Maps.Interface.Types
  ( module Kernel.External.Maps.Interface.Types,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Control.Lens.Operators
import qualified Data.Aeson as A
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC
import Data.Geospatial
import Data.LineString
import Data.OpenApi hiding (components, description)
import qualified Data.OpenApi as OpenApi
import Data.Text
import qualified Data.Text.Encoding as TE
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Debug.Trace as T
import Deriving.Aeson
import EulerHS.Prelude
import qualified Kernel.External.Maps.Google.Config as Google
import qualified Kernel.External.Maps.MMI.Config as MMI
import qualified Kernel.External.Maps.NextBillion.Config as NextBillion
import qualified Kernel.External.Maps.OSRM.Config as OSRM
import Kernel.External.Maps.Types
import Kernel.External.Types (Language)
import Kernel.Types.Common
import Kernel.Utils.GenericPretty (PrettyShow)

data SnapToRoadHandler m = SnapToRoadHandler
  { getProvidersList :: m [MapsService],
    getConfidenceThreshold :: m Double,
    getProviderConfig :: MapsService -> m MapsServiceConfig
  }

data MapsServiceConfig = GoogleConfig Google.GoogleCfg | OSRMConfig OSRM.OSRMCfg | MMIConfig MMI.MMICfg | NextBillionConfig NextBillion.NextBillionCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] MapsServiceConfig

data TravelMode = CAR | MOTORCYCLE | BICYCLE | FOOT
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data GetDistanceReq a b = GetDistanceReq
  { origin :: a,
    destination :: b,
    distanceUnit :: DistanceUnit,
    travelMode :: Maybe TravelMode
  }
  deriving (Generic, Show)

data GetDistancesReq a b = GetDistancesReq
  { origins :: NonEmpty a,
    destinations :: NonEmpty b,
    distanceUnit :: DistanceUnit,
    travelMode :: Maybe TravelMode
  }
  deriving (Generic, Show)

data GetDistanceResp a b = GetDistanceResp
  { origin :: a,
    destination :: b,
    distance :: Meters,
    distanceWithUnit :: Distance,
    duration :: Seconds,
    status :: Text
  }
  deriving (Generic, Show, PrettyShow, FromJSON)

type GetDistancesResp a b = NonEmpty (GetDistanceResp a b)

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
    distanceWithUnit :: Maybe Distance,
    boundingBox :: Maybe BoundingBoxWithoutCRS,
    snappedWaypoints :: [LatLong],
    points :: [LatLong]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, Eq, Ord)

instance FromField RouteInfo where
  fromField f mbValue = case mbValue of
    Nothing -> DPSF.returnError UnexpectedNull f mempty
    Just value' ->
      case T.trace ("text val" <> show value') $ A.eitherDecode $ BL.fromStrict value' of
        Right jsonVal -> case T.trace ("text val" <> show value') $ A.eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonVal) of
          Right val -> pure val
          _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')
        _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

instance FromBackendRow Postgres RouteInfo

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be RouteInfo where
  sqlValueSyntax = sqlValueSyntax . (stringify . A.String . stringify . A.toJSON)
    where
      stringify = TE.decodeUtf8 . BLC.toStrict . A.encode

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RouteInfo

deriving stock instance Ord BoundingBoxWithoutCRS

deriving stock instance Ord PointXY

deriving stock instance Ord PointXYZ

deriving stock instance Ord PointXYZM

deriving stock instance Read BoundingBoxWithoutCRS

deriving stock instance Read PointXY

deriving stock instance Read PointXYZ

deriving stock instance Read PointXYZM

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
    distanceUnit :: DistanceUnit
  }
  deriving stock (Generic)
  deriving (Show)

-- deriving anyclass (FromJSON, ToJSON, ToSchema)

data SnapToRoadResp = SnapToRoadResp
  { distance :: HighPrecMeters,
    distanceWithUnit :: Distance,
    confidence :: Double,
    snappedPoints :: [LatLong]
  }
  deriving stock (Generic)
  deriving (Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Country = India | France | USA
  deriving stock (Generic)
  deriving (Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AutoCompleteReq = AutoCompleteReq
  { input :: Text,
    sessionToken :: Maybe Text,
    location :: Text,
    radius :: Integer,
    radiusWithUnit :: Maybe Distance,
    language :: Language,
    strictbounds :: Maybe Bool,
    origin :: Maybe LatLong,
    types_ :: Maybe Text,
    country :: Country
  }
  deriving stock (Generic)
  deriving (Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype AutoCompleteResp = AutoCompleteResp
  { predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Prediction = Prediction
  { description :: Text,
    placeId :: Maybe Text,
    distance :: Maybe Int,
    distanceWithUnit :: Maybe Distance,
    types :: Maybe [Text]
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
