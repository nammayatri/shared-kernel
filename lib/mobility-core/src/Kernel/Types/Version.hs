{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Kernel.Types.Version where

import Control.Lens
import Data.OpenApi (OpenApiType (OpenApiString), ToParamSchema (..))
import Data.OpenApi.Lens as L
import Data.Text as T
import Kernel.Prelude as Prelude
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.Regex

data Version = Version
  { major :: Int,
    minor :: Int,
    maintenance :: Int,
    preRelease :: Maybe Text,
    build :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

versionToText :: Version -> Text
versionToText Version {..} =
  pack (show major) <> "." <> pack (show minor) <> "." <> pack (show maintenance)
    <> maybe "" ("-" <>) preRelease
    <> maybe "" ("+" <>) build

textToVersion :: Text -> Either Text Version
textToVersion versionText =
  let versionStr = unpack versionText
      regex = mkRegex "^([0-9]+)\\.([0-9]+)\\.([0-9]+)(-[^+]+)?(\\+.+)?$"
      matchResult = matchRegex regex versionStr
   in case matchResult of
        Just [majorStr, minorStr, maintenanceStr, preReleaseStr, buildStr] -> do
          let mbMajor = readMaybe majorStr
          let mbMinor = readMaybe minorStr
          let mbMaintenance = readMaybe maintenanceStr
          let preRelease = if preReleaseStr /= "" then Just (T.tail $ pack preReleaseStr) else Nothing -- Tail to remove the leading '-'
          let build = if buildStr /= "" then Just (T.tail $ pack buildStr) else Nothing -- Tail to remove the leading '+'
          case (mbMajor, mbMinor, mbMaintenance) of
            (Just major, Just minor, Just maintenance) -> Right Version {..}
            _ -> Left "Invalid version number"
        _ -> Left "Version does not match expected format"

instance ToParamSchema Version where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & L.pattern ?~ "^\\d+.\\d+.\\d+"

instance ToHttpApiData Version where
  toUrlPiece = versionToText

instance FromHttpApiData Version where
  parseUrlPiece = textToVersion
