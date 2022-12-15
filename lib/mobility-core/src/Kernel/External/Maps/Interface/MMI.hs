module Kernel.External.Maps.Interface.MMI
  ( autoSuggest,
    getDistanceMatrix,
  )
where

import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Text as T
import Kernel.External.Encryption
import EulerHS.Prelude
import Kernel.External.Maps.Types
import Kernel.External.Maps.Interface.Types as IT
import Kernel.External.Maps.MMI.AutoSuggest as MMI
import Kernel.External.Maps.MMI.DistanceMatrix as MMI
import Kernel.External.Maps.MMI.Config
import Kernel.External.Maps.HasCoordinates (HasCoordinates (..))
import Kernel.External.Maps.MMI.MMIAuthToken as MMIAuthToken
import qualified Kernel.External.Maps.MMI.MapsClient.Types as MMI
import qualified Kernel.External.Maps.MMI.MapsClient.Types as MMITypes
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing

autoSuggest ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  IT.AutoCompleteReq ->
  m IT.AutoCompleteResp
autoSuggest mmiCfg AutoCompleteReq {..} = do
  let query = input
      loc = location
      region = "ind"
      lang = language
      mapsUrl = mmiCfg.mmiNonKeyUrl
  token <- MMIAuthToken.getTokenText mmiCfg
  res <- MMI.mmiAutoSuggest mapsUrl (Just $ MMITypes.MMIAuthToken token) query loc region lang
  let predictions = map (\MMITypes.SuggestedLocations {..} -> Prediction {placeId = Just eLoc, description = placeName <> " " <> placeAddress}) res.suggestedLocations
  return $ AutoCompleteResp predictions

getDistanceMatrix ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MMICfg ->
  IT.GetDistancesReq a b ->
  m (NonEmpty (IT.GetDistanceResp a b))
getDistanceMatrix mmiCfg GetDistancesReq {..} = do
  key <- decrypt mmiCfg.mmiApiKey
  let limitedOriginObjectsList = splitListByAPICap origins
      limitedDestinationObjectsList = splitListByAPICap destinations
      lOrigin = length limitedOriginObjectsList
      lDest = length limitedDestinationObjectsList
      mapsUrl = mmiCfg.mmiKeyUrl
  res <- concatForM limitedOriginObjectsList $ \limitedOriginObjects ->
    concatForM limitedDestinationObjectsList $ \limitedDestinationObjects -> do
      let limitedOriginPlaces = map getCoordinates limitedOriginObjects
          limitedDestinationPlaces = map getCoordinates limitedDestinationObjects
          strOrig = map show [0 .. (lOrigin - 1)]
          strDest = map show [0 .. (lDest - 1)]
          origParam = T.intercalate ";" strOrig
          origDest = T.intercalate ";" strDest
          placesList = (++) limitedOriginPlaces limitedDestinationPlaces
          coordinatesList = map latLongToText placesList
          coordinates = T.intercalate ";" coordinatesList
      MMI.mmiDistanceMatrix mapsUrl key coordinates (Just origParam) (Just origDest)
        >>= parseDistanceMatrixResp lOrigin lDest limitedOriginObjects limitedDestinationObjects
  case res of
    [] -> throwError (InternalError "Empty MMI.getDistances result.")
    (a : xs) -> return $ a :| xs
  where
    splitListByAPICap inputList = do
      List.chunksOf 50 $ toList inputList

latLongToText :: LatLong -> Text
latLongToText LatLong {..} = show lon <> "," <> show lat

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- orig, dest, distance, duration, status
parseDistanceMatrixResp ::
  (MonadThrow m, MonadIO m, Log m) =>
  Int ->
  Int ->
  [a] ->
  [b] ->
  MMITypes.DistanceMatrixResp ->
  m [IT.GetDistanceResp a b]
parseDistanceMatrixResp lOrigin lDest listSrc listDest distanceMatrixResp = do
  let lst = cartProd [0 .. (lOrigin -1)] [0 .. (lDest -1)]
  return $ map (buildResp listSrc listDest distanceMatrixResp) lst

buildResp ::
  [a] ->
  [b] ->
  MMITypes.DistanceMatrixResp ->
  (Int, Int) ->
  IT.GetDistanceResp a b
buildResp listSrc listDest distanceMatrixResp pair =
  GetDistanceResp
    { origin = listSrc !! fst pair,
      destination = listDest !! snd pair,
      distance = (distanceMatrixResp.results.distances !! fst pair) !! snd pair,
      duration = (distanceMatrixResp.results.durations !! fst pair) !! snd pair,
      status = distanceMatrixResp.results.code
    }
