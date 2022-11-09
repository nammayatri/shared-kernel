{-# LANGUAGE DerivingVia #-}

module Beckn.External.GoogleMaps.Types where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Data.Double.Conversion.Text (toFixed)
import qualified Data.Text as T
import Servant (ToHttpApiData (toUrlPiece))

type HasGoogleMaps m r = (MonadReader r m, HasField "googleMapsUrl" r BaseUrl, HasField "googleMapsKey" r Text)

data SearchLocationResp = SearchLocationResp
  { status :: Text,
    predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Prediction = Prediction
  { description :: Text,
    place_id :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlaceDetailsResp = PlaceDetailsResp
  { status :: Text,
    result :: PlaceDetailsResult
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PlaceDetailsResult = PlaceDetailsResult
  { geometry :: Geometry
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Geometry = Geometry
  { location :: LocationS
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationS = LocationS
  { lat :: Double,
    lng :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData LocationS where
  toUrlPiece (LocationS lat' lng') =
    T.concat [toFixed precision lat', ",", toFixed precision lng']
    where
      precision = 6 -- Precision beyond 6 decimal places is ignored.

data GetPlaceNameResp = GetPlaceNameResp
  { status :: Text,
    results :: [ResultsResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResultsResp = ResultsResp
  { formatted_address :: Maybe Text,
    address_components :: [AddressResp],
    plus_code :: Maybe PlusCodeResp,
    geometry :: Geometry,
    types :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddressResp = AddressResp
  { long_name :: Text,
    short_name :: Text,
    types :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype PlusCodeResp = PlusCodeResp
  { compound_code :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DirectionsResp = DirectionsResp
  { routes :: [Route],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Route = Route
  { bounds :: Bounds,
    legs :: [Leg]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Bounds = Bounds
  { northeast :: LocationS,
    southwest :: LocationS
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Leg = Leg
  { distance :: TextValue,
    duration :: TextValue,
    end_location :: LocationS,
    start_location :: LocationS,
    steps :: [Step]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Step = Step
  { distance :: TextValue,
    duration :: TextValue,
    end_location :: LocationS,
    polyline :: EncodedPointObject,
    start_location :: LocationS,
    travel_mode :: Mode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype EncodedPointObject = EncodedPointObject {points :: Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DistanceMatrixResp = DistanceMatrixResp
  { destination_addresses :: [Text],
    origin_addresses :: [Text],
    rows :: [DistanceMatrixRow],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype DistanceMatrixRow = DistanceMatrixRow {elements :: [DistanceMatrixElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data DistanceMatrixElement = DistanceMatrixElement
  { distance :: Maybe TextValue,
    duration :: Maybe TextValue,
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data TextValue = TextValue
  { text :: Text,
    value :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Place = Location LocationS | Address Text

instance ToHttpApiData Place where
  toUrlPiece (Location location) = toUrlPiece location
  toUrlPiece (Address address) = address

instance ToHttpApiData [Place] where
  toUrlPiece latLongList = T.intercalate "|" $ toUrlPiece <$> latLongList

data Mode = DRIVING | WALKING | BICYCLING | TRANSIT
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData Mode where
  toUrlPiece = T.toLower . show

data Language
  = ENGLISH
  | HINDI
  | KANNADA
  | TAMIL
  | MALAYALAM
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)
  deriving (PrettyShow) via Showable Language

instance ToHttpApiData Language where
  toUrlPiece ENGLISH = "en"
  toUrlPiece HINDI = "hi"
  toUrlPiece KANNADA = "kn"
  toUrlPiece MALAYALAM = "ml"
  toUrlPiece TAMIL = "ta"

toMbLanguage :: Maybe Text -> Maybe Language
toMbLanguage txt =
  case txt of
    Nothing -> Nothing
    Just "en" -> Just ENGLISH
    Just "hi" -> Just HINDI
    Just "kn" -> Just KANNADA
    Just "ml" -> Just MALAYALAM
    Just "ta" -> Just TAMIL
    Just _ -> Nothing

data DepartureTime = Now | FutureTime UTCTime

instance ToHttpApiData DepartureTime where
  toUrlPiece Now = "now"
  toUrlPiece (FutureTime time) = show time
