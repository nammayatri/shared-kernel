module Kernel.Utils.Geometry where

import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process
import Text.Regex

extractGeometry :: String -> Maybe String
extractGeometry shapeData =
  case matchRegex geometryPattern shapeData of
    Just [_, geometry] -> Just geometry
    _ -> Nothing
  where
    geometryPattern = mkRegex "INSERT INTO .* VALUES \\(.*'(.*)'\\);"

getGeomFromKML :: (MonadFlow m) => FilePath -> m (Maybe String)
getGeomFromKML kmlFilePath = do
  currentDir <- L.runIO $ getCurrentDirectory
  let tempDirPath = currentDir </> "temp"
      geoJsonFilePath = tempDirPath </> "output.json"
      shpFilePath = tempDirPath </> "output.shp"
      sqlFilePath = tempDirPath </> "output.shp"
  _ <- L.runIO $ createDirectoryIfMissing True tempDirPath
  let kmlToGeoJSON = "ogr2ogr -f GeoJSON " ++ geoJsonFilePath ++ " " ++ kmlFilePath
      geoJsonToShp = "ogr2ogr -f \"ESRI Shapefile\" " ++ shpFilePath ++ " " ++ geoJsonFilePath
      shpToPgsql = "shp2pgsql " ++ shpFilePath ++ " > " ++ sqlFilePath
  _ <- L.runIO $ callCommand kmlToGeoJSON
  _ <- L.runIO $ callCommand geoJsonToShp
  _ <- L.runIO $ callCommand shpToPgsql
  sqlFile <- L.runIO $ readFile sqlFilePath
  _ <- L.runIO $ removeDirectoryRecursive tempDirPath
  return $ extractGeometry sqlFile
