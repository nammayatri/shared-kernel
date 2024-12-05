module Kernel.External.Weather.Types where

import Data.Aeson.Types
import Kernel.External.Weather.OpenWeatherMap.Config as OWM
import Kernel.Prelude
import Kernel.Utils.JSON

data Coordinates = Coordinates
  { lon :: Float,
    lat :: Float
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data WeatherCondition = WeatherCondition
  { id :: Int,
    main :: Text,
    description :: Text,
    icon :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data MainWeather = MainWeather
  { temp :: Float,
    feels_like :: Float,
    temp_min :: Float,
    temp_max :: Float,
    pressure :: Int,
    humidity :: Int,
    sea_level :: Int,
    grnd_level :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Wind = Wind
  { speed :: Float,
    deg :: Int,
    gust :: Float
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Rain = Rain
  { _1h :: Float
  }
  deriving (Show, Generic)

instance FromJSON Rain where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Rain where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype Cloud = Cloud
  { _all :: Int
  }
  deriving (Show, Generic)

instance FromJSON Cloud where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Cloud where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data SysWeather = SysWeather
  { _type :: Int,
    id :: Int,
    country :: Text,
    sunrise :: Int,
    sunset :: Int
  }
  deriving (Show, Generic)

instance FromJSON SysWeather where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON SysWeather where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data WeatherResponse = WeatherResponse
  { coord :: Coordinates,
    weather :: [WeatherCondition],
    main :: MainWeather,
    visibility :: Int,
    wind :: Wind,
    rain :: Rain,
    clouds :: Cloud,
    dt :: Int,
    sys :: SysWeather,
    name :: Text,
    timezone :: Int,
    id :: Int,
    cod :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data WeatherReq = WeatherReq
  { latitude :: Text,
    longitude :: Text
  }

data WeatherServiceConfig = OpenWeatherMapConfig OWM.OpenWeatherCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
