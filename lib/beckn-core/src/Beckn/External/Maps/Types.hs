{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.External.Maps.Types
  ( module Beckn.External.Maps.Types,
    module Data.Geospatial,
    module Data.LineString,
  )
where

import Beckn.Utils.GenericPretty (PrettyShow, Showable (Showable))
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

data Language
  = ENGLISH
  | HINDI
  | KANNADA
  | TAMIL
  | MALAYALAM
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)
  deriving (PrettyShow) via Showable Language

instance FromHttpApiData Language where
  parseUrlPiece "en" = pure ENGLISH
  parseUrlPiece "hi" = pure HINDI
  parseUrlPiece "kn" = pure KANNADA
  parseUrlPiece "ml" = pure MALAYALAM
  parseUrlPiece "ta" = pure TAMIL
  parseUrlPiece _ = Left "Unable to parse Language"

instance ToHttpApiData Language where
  toUrlPiece ENGLISH = "en"
  toUrlPiece HINDI = "hi"
  toUrlPiece KANNADA = "kn"
  toUrlPiece MALAYALAM = "ml"
  toUrlPiece TAMIL = "ta"

data GetPlaceNameBy = ByLatLong LatLong | ByPlaceId Text
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
