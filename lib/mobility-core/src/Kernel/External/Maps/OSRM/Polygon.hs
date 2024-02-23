module Kernel.External.Maps.OSRM.Polygon where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Ext
import Data.Geometry
import Data.Geometry.Polygon (fromPoints)
import qualified Data.HashMap.Strict as HM
import Data.Text as T hiding (any, concatMap, map)
import Kernel.External.Maps.OSRM.Config
import Kernel.External.Maps.Types
import Kernel.Prelude hiding (False, True, putStrLn, show)
import Kernel.Types.Error
import Kernel.Utils.Common hiding (show)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName, (</>))
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (False, True)

data GeoJSON = GeoJSON
  { geoType :: Text,
    coordinates :: [[[(Double, Double)]]]
  }
  deriving (Generic, Show)

type GeoConfigPath = FilePath

instance FromJSON GeoJSON where
  parseJSON = withObject "GeoJSON" $ \obj -> do
    geoType <- obj .: "type"
    coordinates <- obj .: "coordinates"
    return $ GeoJSON geoType coordinates

toSimplePolygon :: [[(Double, Double)]] -> SimplePolygon () Double
toSimplePolygon = fromPoints . concatMap (map toPoint)
  where
    toPoint :: (Double, Double) -> Point 2 Double :+ ()
    toPoint (x, y) = Point2 x y :+ ()

getCity :: LatLong -> IO (Maybe Text)
getCity (LatLong lat lon) = do
  let pointToCheck = Point2 lon lat
  geoConfigPath <- getGeoConfigPath
  filePathsAndGeoJSONs <- readGeoPolygons geoConfigPath
  result <- findCity pointToCheck filePathsAndGeoJSONs
  pure result

findCity :: Point 2 Double -> [(FilePath, Maybe GeoJSON)] -> IO (Maybe Text)
findCity _ [] = return Nothing
findCity point ((filePath, maybeGeoJSON) : rest) = do
  case checkCity point filePath maybeGeoJSON of
    Just cityName -> return (Just cityName)
    Nothing -> findCity point rest

checkCity :: Point 2 Double -> FilePath -> Maybe GeoJSON -> Maybe Text
checkCity _ _ Nothing = Nothing
checkCity point filePath (Just geoJSON) =
  let cityName = T.pack $ takeBaseName filePath
      polygons = map toSimplePolygon (coordinates geoJSON)
   in if any (pointInsideOrOnBoundary point) polygons
        then Just cityName
        else Nothing

readGeoPolygons :: GeoConfigPath -> IO [(FilePath, Maybe GeoJSON)]
readGeoPolygons configPath = do
  files <- listDirectory configPath
  forM files $ \fileName -> do
    let filePath = configPath </> fileName
    content <- readGeoJSONFile filePath
    return (filePath, content)

readGeoJSONFile :: FilePath -> IO (Maybe GeoJSON)
readGeoJSONFile filePath =
  catch
    ( do
        geoJSON <- BS.readFile filePath
        case decode geoJSON of
          Just parsed -> return $ Just parsed
          Nothing -> return Nothing
    )
    (\e -> if isDoesNotExistError e then return Nothing else ioError e)

getGeoConfigPath :: IO GeoConfigPath
getGeoConfigPath = fromMaybe "./geo_config" <$> lookupEnv "GEO_CONFIG"

-- Function to check if a point is inside or on the boundary of a polygon
pointInsideOrOnBoundary :: Point 2 Double -> SimplePolygon () Double -> Bool
pointInsideOrOnBoundary point polygon = point `insidePolygon` polygon || point `onBoundary` polygon

fetchOSRMRegionUrl :: (MonadFlow m) => [LatLong] -> OSRMShardedCfg -> m BaseUrl
fetchOSRMRegionUrl wps osrmShardCfg = do
  wps' <- headMaybe wps
  city <- liftIO $ getCity wps'
  case city of
    Just cityName -> case HM.lookup cityName (osrmShardCfg.cityToRegionUrlMap) of
      Just osrmShardUrl -> pure osrmShardUrl
      Nothing -> pure (fromJust $ HM.lookup "osrmUrl" (osrmShardCfg.cityToRegionUrlMap))
    Nothing -> pure (fromJust $ HM.lookup "osrmUrl" (osrmShardCfg.cityToRegionUrlMap))
  where
    headMaybe [] = throwError $ InternalError "Empty WayPoints list"
    headMaybe (x : _) = pure x
