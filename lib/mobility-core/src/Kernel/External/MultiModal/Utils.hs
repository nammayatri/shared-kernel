{-# LANGUAGE BangPatterns #-}

module Kernel.External.MultiModal.Utils
  ( convertGoogleToGeneric,
    convertOTPToGeneric,
    decode,
    encode,
  )
where

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.List (nub, partition, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Debug.Trace as DT
import EulerHS.Prelude (liftA2, liftA3, safeHead)
import GHC.Float (int2Double)
import Kernel.External.Maps.Google.MapsClient.Types as GT
import Kernel.External.Maps.Google.PolyLinePoints (oneCoordEnc, stringToCoords)
import Kernel.External.MultiModal.Interface.Types
import Kernel.External.MultiModal.OpenTripPlanner.Config (MultiModalWeightedSortCfg (..), validateWeightedSortCfg)
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTP
import Kernel.Prelude
import qualified Kernel.Types.Distance as Distance
import qualified Kernel.Types.Time as Time
import Kernel.Utils.Time (millisecondsToUTC, parseISO8601UTC, utcToEpochSeconds)

extractDuration :: T.Text -> Int
extractDuration t = read (filter Char.isDigit (T.unpack t)) :: Int

makePairs :: [Double] -> [GT.LatLngV2]
makePairs (d1 : d2 : ds) = GT.LatLngV2 d1 d2 : makePairs ds
makePairs [] = []
makePairs _ = []

catPairs :: [GT.LatLngV2] -> [Double]
catPairs [] = []
catPairs (GT.LatLngV2 a b : xs) = a : b : catPairs xs

addPair :: GT.LatLngV2 -> GT.LatLngV2 -> GT.LatLngV2
addPair (GT.LatLngV2 x1 y1) (GT.LatLngV2 x2 y2) = GT.LatLngV2 (x1 + x2) (y1 + y2)

subPair :: GT.LatLngV2 -> GT.LatLngV2 -> GT.LatLngV2
subPair (GT.LatLngV2 x1 y1) (GT.LatLngV2 x2 y2) = GT.LatLngV2 (x1 - x2) (y1 - y2)

adjDiff :: [GT.LatLngV2] -> [GT.LatLngV2]
adjDiff p = zipWith subPair p (GT.LatLngV2 0 0 : p)

decode :: T.Text -> [GT.LatLngV2]
decode = scanl1 addPair . makePairs . stringToCoords . TE.encodeUtf8

encode :: [GT.LatLngV2] -> T.Text
encode = T.concat . fmap oneCoordEnc . catPairs . adjDiff

convertModeToGeneral :: OTP.Mode -> GeneralVehicleType
convertModeToGeneral OTP.ModeBUS = Bus
convertModeToGeneral OTP.ModeRAIL = MetroRail
convertModeToGeneral OTP.ModeMONORAIL = MetroRail
convertModeToGeneral OTP.ModeSUBWAY = Subway
convertModeToGeneral OTP.ModeWALK = Walk
convertModeToGeneral _ = Unspecified

convertTransitVehicleToGeneral :: GT.TransitVehicleTypeV2 -> GeneralVehicleType
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_BUS = Bus
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_HEAVY_RAIL = MetroRail
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_HIGH_SPEED_TRAIN = MetroRail
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_LONG_DISTANCE_TRAIN = MetroRail
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_METRO_RAIL = MetroRail
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_MONORAIL = MetroRail
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_RAIL = MetroRail
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_SUBWAY = Subway
convertTransitVehicleToGeneral _ = Unspecified

convertGoogleToGeneric :: GT.AdvancedDirectionsResp -> MultiModalResponse
convertGoogleToGeneric gResponse =
  let gRoutes = gResponse.routes
      genericRoutes = foldr accumulateRoutes [] gRoutes
   in MultiModalResponse
        { routes = genericRoutes
        }
  where
    accumulateRoutes :: GT.RouteV2 -> [MultiModalRoute] -> [MultiModalRoute]
    accumulateRoutes gRoute genericRoutes =
      let routeDuration = extractDuration gRoute.duration
          routeDistance =
            Distance.Distance
              { value =
                  Distance.HighPrecDistance
                    { getHighPrecDistance = toRational gRoute.distanceMeters
                    },
                unit = Distance.Meter
              }
          gLegs = gRoute.legs
          routeLegs = adjustWalkingLegs $ mergeWalkingLegs $ foldr accumulateLegs [] gLegs
       in MultiModalRoute
            { distance = routeDistance,
              duration = Time.Seconds routeDuration,
              legs = routeLegs,
              startTime = Nothing,
              endTime = Nothing,
              relevanceScore = Nothing
            } :
          genericRoutes
    accumulateLegs :: GT.LegV2 -> [MultiModalLeg] -> [MultiModalLeg]
    accumulateLegs gLeg genericLegs =
      let gSteps = gLeg.steps
       in genericLegs ++ foldr accumulateSteps [] gSteps
    accumulateSteps :: GT.StepV2 -> [MultiModalLeg] -> [MultiModalLeg]
    accumulateSteps gStep genericLegs =
      let stepDuration = extractDuration gStep.staticDuration
          stepDistance =
            Distance.Distance
              { value =
                  Distance.HighPrecDistance
                    { getHighPrecDistance = toRational gStep.distanceMeters
                    },
                unit = Distance.Meter
              }
          stepPolyline = GT.encodedPolyline gStep.polyline
          (stepTravelMode, genericAgency, fromStopDetails', toStopDetails', fromArrivalTime', fromDepartureTime', toArrivalTime', toDepartureTime') = case gStep.travelMode of
            GT.TRANSIT ->
              case gStep.transitDetails of
                Just details ->
                  let travelM = convertTransitVehicleToGeneral details.transitLine.vehicle._type
                      gAgency = safeHead details.transitLine.agencies
                      eName = details.stopDetails.arrivalStop.name
                      sName = details.stopDetails.departureStop.name
                      fDepartureTime = parseISO8601UTC $ T.unpack details.stopDetails.departureTime
                      tArrivalTime = parseISO8601UTC $ T.unpack details.stopDetails.arrivalTime
                      fromDetails =
                        MultiModalStopDetails
                          { stopCode = Nothing,
                            name = Just sName,
                            gtfsId = Nothing,
                            platformCode = Nothing
                          }
                      toDetails =
                        MultiModalStopDetails
                          { stopCode = Nothing,
                            name = Just eName,
                            gtfsId = Nothing,
                            platformCode = Nothing
                          }
                      generAgency = case gAgency of
                        Just x ->
                          Just
                            MultiModalAgency
                              { gtfsId = Nothing,
                                name = x.name
                              }
                        Nothing -> Nothing
                   in (travelM, generAgency, Just fromDetails, Just toDetails, Nothing, fDepartureTime, tArrivalTime, Nothing)
                Nothing -> (Unspecified, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            _ -> (Walk, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
          (startLocationLat, startLocationLng) = (gStep.startLocation.latLng.latitude, gStep.startLocation.latLng.longitude)
          (endLocationLat, endLocationLng) = (gStep.endLocation.latLng.latitude, gStep.endLocation.latLng.longitude)
       in MultiModalLeg
            { distance = stepDistance,
              duration = Time.Seconds stepDuration,
              polyline =
                GT.Polyline
                  { encodedPolyline = stepPolyline
                  },
              mode = stepTravelMode,
              fromStopDetails = fromStopDetails',
              toStopDetails = toStopDetails',
              startLocation =
                GT.LocationV2
                  { latLng =
                      GT.LatLngV2
                        { latitude = startLocationLat,
                          longitude = startLocationLng
                        }
                  },
              endLocation =
                GT.LocationV2
                  { latLng =
                      GT.LatLngV2
                        { latitude = endLocationLat,
                          longitude = endLocationLng
                        }
                  },
              agency = genericAgency,
              serviceTypes = [],
              fromArrivalTime = fromArrivalTime',
              fromDepartureTime = fromDepartureTime',
              toArrivalTime = toArrivalTime',
              toDepartureTime = toDepartureTime',
              routeDetails = []
            } :
          genericLegs
    mergeWalkingLegs :: [MultiModalLeg] -> [MultiModalLeg]
    mergeWalkingLegs [] = []
    mergeWalkingLegs (leg : legs) = mergeLeg leg legs
      where
        mergeLeg currentLeg [] = [currentLeg]
        mergeLeg currentLeg (nextLeg : restLegs)
          | currentLeg.mode == Walk && nextLeg.mode == Walk =
            mergeLeg (mergeTwoLegs currentLeg nextLeg) restLegs
          | otherwise = currentLeg : mergeLeg nextLeg restLegs

    mergeTwoLegs :: MultiModalLeg -> MultiModalLeg -> MultiModalLeg
    mergeTwoLegs leg1 leg2 =
      let leg1Start = leg1.startLocation
          leg2Start = leg2.startLocation
          leg2End = leg2.endLocation
          encodedPolylineText = encode [leg1Start.latLng, leg2Start.latLng, leg2End.latLng]
       in MultiModalLeg
            { distance =
                Distance.Distance
                  { value =
                      Distance.HighPrecDistance
                        { getHighPrecDistance = fromRational leg1.distance.value.getHighPrecDistance + fromRational leg2.distance.value.getHighPrecDistance
                        },
                    unit = leg1.distance.unit
                  },
              duration = Time.Seconds $ leg1.duration.getSeconds + leg2.duration.getSeconds,
              polyline = GT.Polyline {encodedPolyline = encodedPolylineText},
              mode = Walk,
              startLocation = leg1Start,
              endLocation = leg2End,
              fromStopDetails = leg1.fromStopDetails,
              toStopDetails = leg2.toStopDetails,
              agency = Nothing,
              serviceTypes = [],
              fromArrivalTime = leg1.fromArrivalTime,
              fromDepartureTime = leg1.fromDepartureTime,
              toArrivalTime = leg2.toArrivalTime,
              toDepartureTime = leg2.toDepartureTime,
              routeDetails = leg1.routeDetails
            }
    adjustWalkingLegs :: [MultiModalLeg] -> [MultiModalLeg]
    adjustWalkingLegs [] = []
    adjustWalkingLegs [leg] = [leg]
    adjustWalkingLegs (leg1 : leg2 : rest) =
      let adjustedLeg1 =
            if leg1.mode == Walk && leg2.mode /= Walk
              then leg1{toStopDetails = leg2.fromStopDetails, toDepartureTime = leg2.fromDepartureTime, toArrivalTime = leg2.fromArrivalTime}
              else leg1
          adjustedLeg2 =
            if leg2.mode == Walk && leg1.mode /= Walk
              then leg2{fromStopDetails = leg1.toStopDetails, fromDepartureTime = leg1.toDepartureTime, fromArrivalTime = leg1.toArrivalTime}
              else leg2
       in adjustedLeg1 : adjustWalkingLegs (adjustedLeg2 : rest)

convertOTPToGeneric :: OTP.OTPPlan -> Distance.Meters -> [GeneralVehicleType] -> Int -> SortingType -> MultiModalWeightedSortCfg -> MultiModalResponse
convertOTPToGeneric otpResponse minimumWalkDistance permissibleModes maxAllowedPublicTransportLegs sortingType relevanceSortCfg =
  let itineraries = otpResponse.plan.itineraries
      (genericRoutes, frequencyMap) = foldr accumulateItineraries ([], HM.empty) itineraries
      mergedRoutes = map mergeConsecutiveMetroLegs genericRoutes
      orderedRoutes = map assignSubLegOrderToRoute mergedRoutes -- Assign subLegOrder here
      updatedRoutes = map (updateRouteAlternateShortNames frequencyMap) orderedRoutes
      filteredRoutes = map (removeShortWalkLegs minimumWalkDistance) updatedRoutes
      filteredByPermissibleModes = filter (hasOnlyPermissibleModes permissibleModes) filteredRoutes
      filteredByMaxPublicTransport = filter (withinMaxAllowedPublicTransportModes maxAllowedPublicTransportLegs) filteredByPermissibleModes
      !_string = DT.trace $ "Filtered by max public transport: " <> show filteredByMaxPublicTransport <> " " <> show maxAllowedPublicTransportLegs <> " " <> show permissibleModes <> " " <> show sortingType <> " " <> show otpResponse <> " " <> show itineraries <> " " <> show genericRoutes <> " " <> show frequencyMap <> " " <> show mergedRoutes <> " " <> show orderedRoutes <> " " <> show updatedRoutes <> " " <> show filteredRoutes <> " " <> show filteredByPermissibleModes
      sortedRoutes = case sortingType of
        Fastest -> sortRoutesByDuration filteredByMaxPublicTransport
        MinimumTransits -> sortRoutesByNumberOfLegs filteredByMaxPublicTransport
        MostRelevant ->
          if validateWeightedSortCfg relevanceSortCfg
            then
              let (onlyWalkItineraries, otherItineraries) = partition (\r -> all (\leg -> leg.mode == Walk) r.legs) filteredByMaxPublicTransport
                  sortedItineraries = sortByRelevance $ addRelevanceScore relevanceSortCfg otherItineraries
               in sortedItineraries <> onlyWalkItineraries
            else filteredByMaxPublicTransport
      finalRoutes = uniqueRoutes sortedRoutes
   in MultiModalResponse
        { routes = finalRoutes
        }
  where
    sortRoutesByDuration :: [MultiModalRoute] -> [MultiModalRoute]
    sortRoutesByDuration = sortBy (\r1 r2 -> compare (r1.duration.getSeconds) (r2.duration.getSeconds))

    sortRoutesByNumberOfLegs :: [MultiModalRoute] -> [MultiModalRoute]
    sortRoutesByNumberOfLegs = sortBy (\r1 r2 -> compare (length r1.legs) (length r2.legs))

    sortByRelevance :: [MultiModalRoute] -> [MultiModalRoute]
    sortByRelevance = sortBy relevanceComparator

    relevanceComparator :: MultiModalRoute -> MultiModalRoute -> Ordering
    relevanceComparator r1 r2 =
      case (r1.relevanceScore, r2.relevanceScore) of
        (Just score1, Just score2) -> compare score1 score2
        (Nothing, Nothing) -> EQ
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT

    calculateRouteDuration :: MultiModalRoute -> Maybe Time.Seconds
    calculateRouteDuration route = Just route.duration

    getArrivalTime :: MultiModalRoute -> Maybe UTCTime
    getArrivalTime route = route.endTime

    getTransfers :: MultiModalRoute -> Maybe Int
    getTransfers route = do
      case (filter (\leg -> leg.mode /= Walk) route.legs) of
        [] -> Nothing
        legs -> Just $ length legs

    calculateNormalizerData :: [MultiModalRoute] -> Maybe NormalizerData
    calculateNormalizerData [] = Nothing
    calculateNormalizerData (firstRoute : routes) = do
      let maxDuration = calculateRouteDuration firstRoute
          minDuration = maxDuration
          maxArrivalTime = getArrivalTime firstRoute
          minArrivalTime = maxArrivalTime
          maxTransfers = getTransfers firstRoute
          minTransfers = maxTransfers
          normalizerDataInit = NormalizerData {..}
      Just $ getData normalizerDataInit
      where
        getData normalizerDataInit =
          foldr
            ( \route normalizerData -> do
                let routeDur = calculateRouteDuration route
                    routeAT = getArrivalTime route
                    routeTf = getTransfers route
                    maxDuration = liftA2 max routeDur normalizerData.maxDuration
                    minDuration = liftA2 min routeDur normalizerData.minDuration
                    maxArrivalTime = liftA2 max routeAT normalizerData.maxArrivalTime
                    minArrivalTime = liftA2 min routeAT normalizerData.minArrivalTime
                    maxTransfers = liftA2 max routeTf normalizerData.maxTransfers
                    minTransfers = liftA2 min routeTf normalizerData.minTransfers
                NormalizerData {..}
            )
            normalizerDataInit
            routes

    normalize :: Int -> Int -> Int -> Maybe Double
    normalize x minVal maxVal = do
      if maxVal < minVal || x < minVal || x > maxVal
        then Nothing
        else
          if maxVal - minVal == 0
            then Just 0
            else Just $ int2Double (x - minVal) / int2Double (maxVal - minVal)

    normalizeSeconds :: Time.Seconds -> Time.Seconds -> Time.Seconds -> Maybe Double
    normalizeSeconds x minVal maxVal = normalize x.getSeconds minVal.getSeconds maxVal.getSeconds

    normalizeUTCTime :: UTCTime -> UTCTime -> UTCTime -> Maybe Double
    normalizeUTCTime x minVal maxVal = do
      let maxVal' = utcToEpochSeconds maxVal
          minVal' = utcToEpochSeconds minVal
          x' = utcToEpochSeconds x
      normalizeSeconds x' minVal' maxVal'

    maxDouble :: Double
    maxDouble = 5

    calculateRelevanceScore :: MultiModalWeightedSortCfg -> NormalizerData -> MultiModalRoute -> Double
    calculateRelevanceScore weight NormalizerData {..} route =
      let routeDur = calculateRouteDuration route
          routeAT = getArrivalTime route
          routeTf = getTransfers route
          normDur :: Maybe Double = join $ liftA3 normalizeSeconds routeDur minDuration maxDuration
          normAT :: Maybe Double = join $ liftA3 normalizeUTCTime routeAT minArrivalTime maxArrivalTime
          normTf :: Maybe Double = join $ liftA3 normalize routeTf minTransfers maxTransfers
          durScore = maybe maxDouble (* weight.duration) normDur
          aTScore = maybe maxDouble (* weight.arrivalTime) normAT
          tfScore = maybe maxDouble (* weight.transfers) normTf
       in durScore + aTScore + tfScore

    addRelevanceScore :: MultiModalWeightedSortCfg -> [MultiModalRoute] -> [MultiModalRoute]
    addRelevanceScore weight routes = do
      maybe
        routes
        ( \normData -> do
            map
              ( \route ->
                  let relevanceScore = calculateRelevanceScore weight normData route
                   in route {relevanceScore = Just relevanceScore}
              )
              routes
        )
        (calculateNormalizerData routes)

    removeShortWalkLegs :: Distance.Meters -> MultiModalRoute -> MultiModalRoute
    removeShortWalkLegs threshold route =
      let thresholdValue = fromIntegral $ Distance.getMeters threshold -- Convert threshold to Double for comparison
          legsWithIndices = zip [0 ..] route.legs -- Pair each leg with its index
          totalLegs = length route.legs
          filteredLegs =
            [ leg
              | (index, leg) <- legsWithIndices,
                not (leg.mode == Walk && getLegDistance leg < thresholdValue && index /= 0 && index /= totalLegs - 1)
            ]
       in route {legs = filteredLegs}

    getLegDistance :: MultiModalLeg -> Double
    getLegDistance leg = fromRational $ leg.distance.value.getHighPrecDistance

    -- Filter routes to include only those with permissible modes
    hasOnlyPermissibleModes :: [GeneralVehicleType] -> MultiModalRoute -> Bool
    hasOnlyPermissibleModes permissibleModesInRoute route =
      all (\leg -> leg.mode `elem` permissibleModesInRoute) route.legs

    -- Filter routes with at most a specified number of legs of Public Transport Type: [Bus, MetroRail]
    withinMaxAllowedPublicTransportModes :: Int -> MultiModalRoute -> Bool
    withinMaxAllowedPublicTransportModes maxAllowed route =
      let publicTransportLegs = filter (\leg -> leg.mode `elem` [Bus, MetroRail, Subway]) route.legs
       in length publicTransportLegs <= maxAllowed

    -- Assign subLegOrder to each routeDetails within every leg
    assignSubLegOrderToRoute :: MultiModalRoute -> MultiModalRoute
    assignSubLegOrderToRoute route =
      let updatedLegs = map assignSubLegOrderToLeg route.legs
       in route {legs = updatedLegs}

    assignSubLegOrderToLeg :: MultiModalLeg -> MultiModalLeg
    assignSubLegOrderToLeg leg =
      let updatedRouteDetails = zipWith (\i rd -> rd {subLegOrder = i}) [1 ..] leg.routeDetails
       in leg {routeDetails = updatedRouteDetails}

    -- Merge consecutive MetroRail legs in a route
    mergeConsecutiveMetroLegs :: MultiModalRoute -> MultiModalRoute
    mergeConsecutiveMetroLegs route =
      let mergedLegs = mergeMetroLegs route.legs
       in route {legs = mergedLegs}

    -- Recursive function to merge consecutive MetroRail legs
    mergeMetroLegs :: [MultiModalLeg] -> [MultiModalLeg]
    mergeMetroLegs [] = []
    mergeMetroLegs [leg] = [leg] -- Single leg, no merging needed
    mergeMetroLegs (leg1 : leg2 : rest)
      | ((leg1.mode == MetroRail && leg2.mode == MetroRail) || ((leg1.mode == Subway && leg2.mode == Subway))) && leg1.agency == leg2.agency =
        let leg1Start = leg1.startLocation
            leg2Start = leg2.startLocation
            leg2End = leg2.endLocation
            encodedPolylineText = encode [leg1Start.latLng, leg2Start.latLng, leg2End.latLng]
            mergedLeg =
              MultiModalLeg
                { distance =
                    Distance.Distance
                      { value =
                          Distance.HighPrecDistance
                            { getHighPrecDistance = fromRational leg1.distance.value.getHighPrecDistance + fromRational leg2.distance.value.getHighPrecDistance
                            },
                        unit = leg1.distance.unit
                      },
                  duration = Time.Seconds $ leg1.duration.getSeconds + leg2.duration.getSeconds,
                  polyline = GT.Polyline {encodedPolyline = encodedPolylineText},
                  mode = leg1.mode,
                  startLocation = leg1.startLocation,
                  endLocation = leg2.endLocation,
                  serviceTypes = nub (leg1.serviceTypes ++ leg2.serviceTypes),
                  fromStopDetails = leg1.fromStopDetails,
                  toStopDetails = leg2.toStopDetails,
                  routeDetails = leg1.routeDetails ++ leg2.routeDetails,
                  agency = leg1.agency,
                  fromArrivalTime = min <$> leg1.fromArrivalTime <*> leg2.fromArrivalTime,
                  fromDepartureTime = min <$> leg1.fromDepartureTime <*> leg2.fromDepartureTime,
                  toArrivalTime = max <$> leg1.toArrivalTime <*> leg2.toArrivalTime,
                  toDepartureTime = max <$> leg1.toDepartureTime <*> leg2.toDepartureTime
                }
         in mergeMetroLegs (mergedLeg : rest) -- Add merged leg and continue
      | otherwise = leg1 : mergeMetroLegs (leg2 : rest) -- Keep leg1, process the rest
    accumulateItineraries :: Maybe OTP.OTPPlanPlanItineraries -> ([MultiModalRoute], HM.HashMap T.Text [T.Text]) -> ([MultiModalRoute], HM.HashMap T.Text [T.Text])
    accumulateItineraries itinerary (genericRoutes, freqMap) =
      case itinerary of
        Nothing -> (genericRoutes, freqMap)
        Just itinerary' ->
          let duration = fromMaybe 0.0 itinerary'.duration
              (legs, distance, updatedFreqMap) = foldr accumulateLegs ([], 0.0, freqMap) itinerary'.legs
              route =
                MultiModalRoute
                  { duration = Time.Seconds $ round duration,
                    distance =
                      Distance.Distance
                        { value =
                            Distance.HighPrecDistance
                              { getHighPrecDistance = toRational distance
                              },
                          unit = Distance.Meter
                        },
                    legs = legs,
                    startTime = (millisecondsToUTC . round) <$> itinerary'.startTime,
                    endTime = (millisecondsToUTC . round) <$> itinerary'.endTime,
                    relevanceScore = Nothing
                  }
           in (route : genericRoutes, updatedFreqMap)

    accumulateLegs :: Maybe OTP.OTPPlanPlanItinerariesLegs -> ([MultiModalLeg], Double, HM.HashMap T.Text [T.Text]) -> ([MultiModalLeg], Double, HM.HashMap T.Text [T.Text])
    accumulateLegs otpLeg (genericLegs, genericDistance, updatedFreqMap) =
      case otpLeg of
        Nothing -> (genericLegs, genericDistance, updatedFreqMap)
        Just otpLeg' ->
          let distance = fromMaybe 0.0 otpLeg'.distance
              duration = fromMaybe 0.0 otpLeg'.duration
              mode = convertModeToGeneral $ fromMaybe OTP.ModeTRANSIT otpLeg'.mode
              startLocName = fmap T.pack $ if fromMaybe "" otpLeg'.from.name == "Origin" then Nothing else otpLeg'.from.name
              endLocName = fmap T.pack $ if fromMaybe "" otpLeg'.to.name == "Destination" then Nothing else otpLeg'.to.name
              encodedPolylineText = T.pack $ maybe "" (\x -> fromMaybe "" x.points) otpLeg'.legGeometry
              (startLat, startLng) = (otpLeg'.from.lat, otpLeg'.from.lon)
              (endLat, endLng) = (otpLeg'.to.lat, otpLeg'.to.lon)
              routeAgency = otpLeg'.route
              serviceTypes = nub $ maybe [] (mapMaybe extractServiceType) (otpLeg'.route >>= \r -> r.trips)
                where
                  extractServiceType (Just trip) =
                    case splitOn "-" (T.unpack $ gtfsIdtoDomainCode $ T.pack trip.gtfsId) of
                      [_prefix, middle, _suffix] -> Just $ T.pack middle
                      _ -> Nothing
                  extractServiceType Nothing = Nothing
              fromArrivalTime' = Just $ millisecondsToUTC $ round otpLeg'.from.arrivalTime
              fromDepartureTime' = Just $ millisecondsToUTC $ round otpLeg'.from.departureTime
              toArrivalTime' = Just $ millisecondsToUTC $ round otpLeg'.to.arrivalTime
              toDepartureTime' = Just $ millisecondsToUTC $ round otpLeg'.to.departureTime
              (fromStopCode, fromStopGtfsId, fromStopPlatformCode) = case otpLeg'.from.stop of
                Just x -> (x.code, Just x.gtfsId, x.platformCode)
                Nothing -> (Nothing, Nothing, Nothing)
              fromStopDetails' =
                if mode == Walk
                  then Nothing
                  else
                    Just
                      MultiModalStopDetails
                        { stopCode = fmap T.pack fromStopCode,
                          name = startLocName,
                          gtfsId = fmap T.pack fromStopGtfsId,
                          platformCode = fmap T.pack fromStopPlatformCode
                        }
              (toStopCode, toStopGtfsId, toStopPlatformCode) = case otpLeg'.to.stop of
                Just x -> (x.code, Just x.gtfsId, x.platformCode)
                Nothing -> (Nothing, Nothing, Nothing)
              toStopDetails' =
                if mode == Walk
                  then Nothing
                  else
                    Just
                      MultiModalStopDetails
                        { stopCode = fmap T.pack toStopCode,
                          name = endLocName,
                          gtfsId = fmap T.pack toStopGtfsId,
                          platformCode = fmap T.pack toStopPlatformCode
                        }
              genericAgency = case routeAgency of
                Nothing -> Nothing
                Just ag ->
                  Just $
                    MultiModalAgency
                      { gtfsId = (\x -> Just $ T.pack x.gtfsId) =<< ag.agency,
                        name = maybe "" (\x -> T.pack x.name) ag.agency
                      }

              routeDetails = case otpLeg'.route of
                Just route ->
                  [ MultiModalRouteDetails
                      { gtfsId = Just $ T.pack route.gtfsId,
                        longName = fmap T.pack route.longName,
                        shortName = fmap T.pack route.shortName,
                        color = fmap T.pack route.color,
                        alternateShortNames = [],
                        fromStopDetails = fromStopDetails',
                        toStopDetails = toStopDetails',
                        startLocation =
                          GT.LocationV2
                            { latLng =
                                GT.LatLngV2
                                  { latitude = startLat,
                                    longitude = startLng
                                  }
                            },
                        endLocation =
                          GT.LocationV2
                            { latLng =
                                GT.LatLngV2
                                  { latitude = endLat,
                                    longitude = endLng
                                  }
                            },
                        subLegOrder = 1,
                        fromArrivalTime = fromArrivalTime',
                        fromDepartureTime = fromDepartureTime',
                        toArrivalTime = toArrivalTime',
                        toDepartureTime = toDepartureTime'
                      }
                  ]
                Nothing -> []

              -- Update the frequency map only if fromStopCode and toStopCode exists
              newFreqMap = case (fromStopCode, toStopCode, otpLeg'.route >>= (.shortName)) of
                (Just fromStopCode', Just toStopCode', Just shortName) ->
                  let key = T.pack fromStopCode' <> "-" <> T.pack toStopCode'
                   in HM.insertWith (\new old -> nub (new ++ old)) key [T.pack shortName] updatedFreqMap
                _ -> updatedFreqMap

              leg =
                MultiModalLeg
                  { distance =
                      Distance.Distance
                        { value =
                            Distance.HighPrecDistance
                              { getHighPrecDistance = toRational distance
                              },
                          unit = Distance.Meter
                        },
                    duration = Time.Seconds $ round duration,
                    polyline =
                      GT.Polyline
                        { encodedPolyline = encodedPolylineText
                        },
                    fromStopDetails = fromStopDetails',
                    toStopDetails = toStopDetails',
                    mode = mode,
                    startLocation =
                      GT.LocationV2
                        { latLng =
                            GT.LatLngV2
                              { latitude = startLat,
                                longitude = startLng
                              }
                        },
                    routeDetails = routeDetails,
                    serviceTypes = serviceTypes,
                    endLocation =
                      GT.LocationV2
                        { latLng =
                            GT.LatLngV2
                              { latitude = endLat,
                                longitude = endLng
                              }
                        },
                    agency = genericAgency,
                    fromArrivalTime = fromArrivalTime',
                    fromDepartureTime = fromDepartureTime',
                    toArrivalTime = toArrivalTime',
                    toDepartureTime = toDepartureTime'
                  }
           in (leg : genericLegs, genericDistance + distance, newFreqMap)

    -- Update frequency of each leg in a route using the frequencyMap
    updateRouteAlternateShortNames :: HM.HashMap T.Text [T.Text] -> MultiModalRoute -> MultiModalRoute
    updateRouteAlternateShortNames freqMap route =
      let updatedLegs = map updateLegAlternateShortNames route.legs
       in route {legs = updatedLegs}
      where
        updateLegAlternateShortNames :: MultiModalLeg -> MultiModalLeg
        updateLegAlternateShortNames leg =
          let updatedRouteDetails = updateDetailsAlternateShortNames freqMap <$> leg.routeDetails
           in leg {routeDetails = updatedRouteDetails}

        updateDetailsAlternateShortNames :: HM.HashMap T.Text [T.Text] -> MultiModalRouteDetails -> MultiModalRouteDetails
        updateDetailsAlternateShortNames frequencyMap details =
          case (details.fromStopDetails >>= (.stopCode), details.toStopDetails >>= (.stopCode)) of
            (Just fromStopCode, Just toStopCode) ->
              let key = fromStopCode <> "-" <> toStopCode
                  shortNames = HM.lookupDefault [] key frequencyMap
               in details {alternateShortNames = shortNames}
            _ -> details

    -- Function to get the sequence combination for a route
    getSequenceCombination :: MultiModalRoute -> T.Text
    getSequenceCombination route =
      let sequenceCombination = concatMap (mapMaybe (\r -> (\f t -> f <> "-" <> t) <$> (r.fromStopDetails >>= (.stopCode)) <*> (r.toStopDetails >>= (.stopCode))) . (.routeDetails)) route.legs
       in T.intercalate "-" sequenceCombination

    -- Function to filter routes with unique sequence combinations
    uniqueRoutes :: [MultiModalRoute] -> [MultiModalRoute]
    uniqueRoutes routes =
      let -- Create a map that tracks sequence combinations
          seenCombinations = Map.empty
          -- Filter routes based on unique sequence combinations
          (newUniqueRoutes, _) =
            foldl
              ( \(accRoutes, seen) route ->
                  let seqComb = getSequenceCombination route
                   in if Map.member seqComb seen
                        then (accRoutes, seen)
                        else (accRoutes ++ [route], Map.insert seqComb () seen)
              )
              ([], seenCombinations)
              routes
       in newUniqueRoutes

gtfsIdtoDomainCode :: Text -> Text
gtfsIdtoDomainCode gtfsId = case break (== ':') $ T.unpack gtfsId of
  (_, ':' : code) -> T.pack code
  _ -> gtfsId
