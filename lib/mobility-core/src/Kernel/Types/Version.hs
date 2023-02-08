module Kernel.Types.Version where

import Kernel.Prelude as Prelude
import Control.Lens
import Data.OpenApi (OpenApiType (OpenApiString), ToParamSchema (..))
import Data.OpenApi.Lens as L
import Data.Text as T
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
