{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}

module Kernel.Types.APISuccess (APISuccess (..)) where

import qualified Control.Lens as L
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import Data.OpenApi
import EulerHS.Prelude

data APISuccess = Success deriving (Generic, Show, Eq)

-- Due to custom toJSON defined here, in the API response this guy goes as something else, and we use openAPI spec to generate frontend code, so toSchema should reflect same as its to json (especially if the object is used in API response)
instance ToSchema APISuccess where
  declareNamedSchema _ = do
    doubleSchema <-
      case A.decode "{\"enum\": [\"Success\"], \"type\": \"string\"}" of
        Just res -> pure res
        Nothing -> declareSchemaRef (Proxy :: Proxy APISuccess)
    return $
      NamedSchema (Just "APISuccess") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ [ ("result", doubleSchema)
                 ]
          & required L..~ ["result"]

instance A.ToJSON APISuccess where
  toJSON Success = A.object ["result" A..= ("Success" :: Text)]

instance A.FromJSON APISuccess where
  parseJSON (A.Object obj) = do
    result :: String <- obj A..: "result"
    case result of
      "Success" -> pure Success
      _ -> parseFail "Expected \"Success\" in \"result\" field."
  parseJSON wrongVal = typeMismatch "Object APISuccess" wrongVal
