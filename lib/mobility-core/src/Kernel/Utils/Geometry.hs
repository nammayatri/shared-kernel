module Kernel.Utils.Geometry where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.JSON
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process
import Text.Regex

data Geometry = Geometry
  { _type :: String,
    coordinates :: [[[Double]]]
  }
  deriving (Generic, Show)

instance FromJSON Geometry where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Geometry where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Feature = Feature
  { _type :: String,
    geometry :: Geometry
  }
  deriving (Generic, Show)

instance FromJSON Feature where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Feature where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data GeoJSON = GeoJSON
  { _type :: String,
    features :: [Feature]
  }
  deriving (Generic, Show)

instance FromJSON GeoJSON where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON GeoJSON where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

convertTo2D :: GeoJSON -> GeoJSON
convertTo2D geoJSON =
  geoJSON {features = map convertFeature2D $ features geoJSON}
  where
    convertFeature2D :: Feature -> Feature
    convertFeature2D feature =
      feature {geometry = convertGeometry2D $ geometry feature}

    convertGeometry2D :: Geometry -> Geometry
    convertGeometry2D geometry =
      geometry {coordinates = map (map convertCoordinate2D) $ coordinates geometry}

    convertCoordinate2D :: [Double] -> [Double]
    convertCoordinate2D coordinate = [head coordinate, head $ tail coordinate]

extractGeometry :: String -> Maybe String
extractGeometry shapeData =
  case matchRegex geometryPattern shapeData of
    Just [geometry] -> Just geometry
    _ -> Nothing
  where
    geometryPattern = mkRegex "INSERT INTO .* VALUES \\(.*'(.*)'\\);"

getGeomFromKML :: (MonadFlow m) => FilePath -> m (Maybe String)
getGeomFromKML kmlFilePath = do
  currentDir <- L.runIO $ getCurrentDirectory
  logInfo $ "Current Directory : " <> T.pack currentDir
  let tempDirPath = currentDir </> "temp"
      geoJson3DFilePath = tempDirPath </> "output_3d.json"
      geoJson2DFilePath = tempDirPath </> "output_2d.json"
      shpFilePath = tempDirPath </> "output.shp"
      sqlFilePath = tempDirPath </> "output.sql"
  void $
    L.runIO $ do
      void $ removeDirectoryRecursiveIfExists tempDirPath
      void $ createDirectoryIfMissing True tempDirPath
  let kmlToGeoJson3D = "ogr2ogr -f GeoJSON " ++ geoJson3DFilePath ++ " " ++ kmlFilePath
      geoJsonToShp = "ogr2ogr -f \"ESRI Shapefile\" " ++ shpFilePath ++ " " ++ geoJson2DFilePath
      shpToPgsql = "shp2pgsql -g geom " ++ shpFilePath ++ " > " ++ sqlFilePath
  _ <- L.runIO $ callCommand kmlToGeoJson3D
  geoJson2D <-
    maybe
      (throwError $ InternalError "Cannot Convert 3D to 2D GeoJSON.")
      (return . Just . convertTo2D)
      =<< decode <$> (L.runIO $ BL.readFile geoJson3DFilePath)
  void $
    L.runIO $ do
      void $ BL.writeFile geoJson2DFilePath (encode geoJson2D)
      void $ callCommand geoJsonToShp
      void $ callCommand shpToPgsql
  sqlFile <- L.runIO $ readFile sqlFilePath
  logInfo $ "SQL Geometry Output : " <> T.pack sqlFile
  _ <- L.runIO $ removeDirectoryRecursiveIfExists tempDirPath
  return $ extractGeometry sqlFile
  where
    removeDirectoryRecursiveIfExists :: FilePath -> IO ()
    removeDirectoryRecursiveIfExists path = do
      exists <- doesDirectoryExist path
      when exists $ removeDirectoryRecursive path
