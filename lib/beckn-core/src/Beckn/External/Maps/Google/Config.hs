module Beckn.External.Maps.Google.Config where

import Beckn.Prelude
import Beckn.Utils.Dhall (FromDhall)

data GoogleCfg = GoogleCfg
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: Text
  }
  deriving (Generic, FromDhall)

type HasGoogleCfg r = HasField "googleCfg" r GoogleCfg
