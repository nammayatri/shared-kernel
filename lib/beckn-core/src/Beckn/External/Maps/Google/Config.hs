module Beckn.External.Maps.Google.Config where

import Beckn.Prelude

data GoogleCfg = GoogleCfg
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
