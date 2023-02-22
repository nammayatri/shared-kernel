 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Version where

import Control.Lens
import Data.OpenApi (OpenApiType (OpenApiString), ToParamSchema (..))
import Data.OpenApi.Lens as L
import Data.Text as T
import Kernel.Prelude as Prelude
import Servant (FromHttpApiData (..), ToHttpApiData (..))

data Version = Version
  { major :: Int,
    minor :: Int,
    maintenance :: Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

versionToText :: Version -> Text
versionToText Version {..} = show major <> "." <> show minor <> "." <> show maintenance

instance ToParamSchema Version where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & L.pattern ?~ "^\\d+.\\d+.\\d+"

instance ToHttpApiData Version where
  toUrlPiece = versionToText

instance FromHttpApiData Version where
  parseUrlPiece versionText = do
    let listOfVersionParts = splitOn "." versionText
    when (Prelude.length listOfVersionParts /= 3) (Left "bad version")
    let [majorString, minorString, maintenanceString] = unpack <$> listOfVersionParts
    let mbMajor = readMaybe majorString
    let mbMinor = readMaybe minorString
    let mbMaintenance = readMaybe maintenanceString
    case (mbMajor, mbMinor, mbMaintenance) of
      (Just major, Just minor, Just maintenance) -> return $ Version major minor maintenance
      (_, _, _) -> Left "bad version"
