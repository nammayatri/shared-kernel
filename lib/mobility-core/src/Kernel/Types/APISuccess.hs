{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.APISuccess (APISuccess (..)) where

import Data.Aeson hiding (Success)
import Data.Aeson.Types (parseFail, typeMismatch)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data APISuccess = Success deriving (Generic, Show, Eq, ToSchema)

instance ToJSON APISuccess where
  toJSON Success = object ["result" .= ("Success" :: Text)]

instance FromJSON APISuccess where
  parseJSON (Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "Success" -> pure Success
      _ -> parseFail "Expected \"Success\" in \"result\" field."
  parseJSON wrongVal = typeMismatch "Object APISuccess" wrongVal
