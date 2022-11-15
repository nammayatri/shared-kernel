{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.External.Maps.Types
  ( module Beckn.External.Maps.Types,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Beckn.Types.Common
import Beckn.Utils.GenericPretty (PrettyShow)
import Control.Lens.Operators
import Data.Geospatial
import Data.LineString
import Data.OpenApi
import Data.Text
import EulerHS.Prelude
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

data LatLong = LatLong
  { lat :: Double,
    lon :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema, PrettyShow)

instance ToParamSchema LatLong where
  toParamSchema _ =
    mempty
      & title ?~ "LatLong"
      & type_ ?~ OpenApiString
      & format ?~ "<latitude>,<longitude>"

instance FromHttpApiData LatLong where
  parseUrlPiece a = do
    let (lat, long) = breakOn a ","
    LatLong <$> readEither lat <*> readEither long

instance ToHttpApiData LatLong where
  toQueryParam LatLong {..} = show lat <> "," <> show lon

data TravelMode = CAR | MOTORCYCLE | BICYCLE | FOOT
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data GetDistanceResult a b = GetDistanceResult
  { origin :: a,
    destination :: b,
    distance :: Meters,
    duration :: Seconds,
    status :: Text
  }
  deriving (Generic, Show, PrettyShow)

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
          & description
            ?~ "https://datatracker.ietf.org/doc/html/rfc7946#section-5"

instance ToSchema GeospatialGeometry where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Value)
    return $
      NamedSchema (Just "GeospatialGeometry") $
        aSchema
          & description
            ?~ "https://datatracker.ietf.org/doc/html/rfc7946#section-2"
