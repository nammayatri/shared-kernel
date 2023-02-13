{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.External.Maps.MMI.MapsClient.Types where

import qualified Data.Text as T
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (constructorsWithSnakeCase)
import Kernel.Utils.TH
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

data AuthRequest = AuthRequest
  { grant_type :: Text,
    client_id :: Text,
    client_secret :: Text
  }
  deriving (Generic, Eq, Show)

instance ToForm AuthRequest where
  toForm AuthRequest {..} =
    [ ("grant_type", toQueryParam grant_type),
      ("client_id", toQueryParam client_id),
      ("client_secret", toQueryParam client_secret)
    ]

data AuthResp = AuthResp
  { accessToken :: Text,
    tokenType :: Text,
    expiresIn :: Int,
    scope :: Text,
    projectCode :: Text,
    clientId :: Text
  }
  deriving (Generic)

instance FromJSON AuthResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON AuthResp where
  toJSON = genericToJSON constructorsWithSnakeCase

newtype MMIAuthToken = MMIAuthToken
  { getMMIAuthToken :: Text
  }
  deriving (Show)
  deriving newtype (PrettyShow)

deriveIdentifierInstances ''MMIAuthToken

data AutoSuggestResp = AutoSuggestResp
  { suggestedLocations :: [SuggestedLocations],
    userAddedLocations :: [UserAddedLocation],
    suggestedSearches :: [SuggestedSearches]
  }
  deriving (Generic, FromJSON, ToJSON)

data SuggestedLocations = SuggestedLocations
  { eLoc :: Text, -- place id in MMI
    placeName :: Text,
    placeAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data UserAddedLocation = UserAddedLocation
  { eLoc :: Text,
    orderIndex :: Int,
    placeAddress :: Text,
    placeName :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data SuggestedSearches = SuggestedSearches
  { keyword :: Text,
    identifier :: Text,
    location :: Text,
    hyperlink :: Text,
    orderIndex :: Int,
    eLoc :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype LatLongList = LatLongList {getLatLongList :: [Maps.LatLong]}

instance ToHttpApiData LatLongList where
  toUrlPiece (LatLongList lst) = T.intercalate ";" $ map convertPoint lst

convertPoint :: Maps.LatLong -> Text
convertPoint (Maps.LatLong lat lon) = show lon <> "," <> show lat

data DistanceMatrixResp = DistanceMatrixResp
  { version :: Text,
    results :: DistanceMatrixResult,
    responseCode :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

data DistanceMatrixResult = DistanceMatrixResult
  { code :: Text,
    distances :: [[Double]],
    durations :: [[Double]]
  }
  deriving (Generic, FromJSON, ToJSON)

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
