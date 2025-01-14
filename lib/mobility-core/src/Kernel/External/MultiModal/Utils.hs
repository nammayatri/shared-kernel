module Kernel.External.MultiModal.Utils
  ( convertGoogleToGeneric,
    convertOTPToGeneric,
    decode,
    encode,
  )
where

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import EulerHS.Prelude (safeHead)
import Kernel.External.Maps.Google.MapsClient.Types as GT
import Kernel.External.Maps.Google.PolyLinePoints (oneCoordEnc, stringToCoords)
import Kernel.External.MultiModal.Interface.Types
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTP
import Kernel.Prelude
import qualified Kernel.Types.Distance as Distance
import qualified Kernel.Types.Time as Time
import Kernel.Utils.Time (millisecondsToUTC, parseISO8601UTC)

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
convertModeToGeneral OTP.ModeSUBWAY = MetroRail
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
convertTransitVehicleToGeneral GT.VEHICLE_TYPE_SUBWAY = MetroRail
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
              endTime = Nothing
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
                            gtfsId = Nothing
                          }
                      toDetails =
                        MultiModalStopDetails
                          { stopCode = Nothing,
                            name = Just eName,
                            gtfsId = Nothing
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
              fromArrivalTime = fromArrivalTime',
              fromDepartureTime = fromDepartureTime',
              toArrivalTime = toArrivalTime',
              toDepartureTime = toDepartureTime',
              routeDetails = [],
              frequency = Nothing
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
              fromArrivalTime = leg1.fromArrivalTime,
              fromDepartureTime = leg1.fromDepartureTime,
              toArrivalTime = leg2.toArrivalTime,
              toDepartureTime = leg2.toDepartureTime,
              routeDetails = leg1.routeDetails,
              frequency = Nothing
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

convertOTPToGeneric :: OTP.OTPPlan -> MultiModalResponse
convertOTPToGeneric otpResponse =
  let itineraries = otpResponse.plan.itineraries
      (genericRoutes, frequencyMap) = foldr accumulateItineraries ([], HM.empty) itineraries
      mergedRoutes = map mergeConsecutiveMetroLegs genericRoutes
      updatedRoutes = map (updateRouteFrequency frequencyMap) mergedRoutes
      finalRoutes = uniqueRoutes updatedRoutes
   in MultiModalResponse
        { routes = finalRoutes
        }
  where
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
      | leg1.mode == MetroRail && leg2.mode == MetroRail && leg1.agency == leg2.agency =
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
                  mode = MetroRail,
                  startLocation = leg1.startLocation,
                  endLocation = leg2.endLocation,
                  fromStopDetails = leg1.fromStopDetails,
                  toStopDetails = leg2.toStopDetails,
                  routeDetails = leg1.routeDetails ++ leg2.routeDetails,
                  agency = leg1.agency,
                  fromArrivalTime = min <$> leg1.fromArrivalTime <*> leg2.fromArrivalTime,
                  fromDepartureTime = min <$> leg1.fromDepartureTime <*> leg2.fromDepartureTime,
                  toArrivalTime = max <$> leg1.toArrivalTime <*> leg2.toArrivalTime,
                  toDepartureTime = max <$> leg1.toDepartureTime <*> leg2.toDepartureTime,
                  frequency = max <$> leg1.frequency <*> leg2.frequency
                }
         in mergeMetroLegs (mergedLeg : rest) -- Add merged leg and continue
      | otherwise = leg1 : mergeMetroLegs (leg2 : rest) -- Keep leg1, process the rest
    accumulateItineraries :: Maybe OTP.OTPPlanPlanItineraries -> ([MultiModalRoute], HM.HashMap T.Text [UTCTime]) -> ([MultiModalRoute], HM.HashMap T.Text [UTCTime])
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
                    endTime = (millisecondsToUTC . round) <$> itinerary'.endTime
                  }
           in (route : genericRoutes, updatedFreqMap)

    accumulateLegs :: Maybe OTP.OTPPlanPlanItinerariesLegs -> ([MultiModalLeg], Double, HM.HashMap T.Text [UTCTime]) -> ([MultiModalLeg], Double, HM.HashMap T.Text [UTCTime])
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
              maybeLongName = otpLeg'.route >>= \r -> r.longName
              fromArrivalTime' = Just $ millisecondsToUTC $ round otpLeg'.from.arrivalTime
              fromDepartureTime' = Just $ millisecondsToUTC $ round otpLeg'.from.departureTime
              toArrivalTime' = Just $ millisecondsToUTC $ round otpLeg'.to.arrivalTime
              toDepartureTime' = Just $ millisecondsToUTC $ round otpLeg'.to.departureTime
              routeDetails = case otpLeg'.route of
                Just route ->
                  [ MultiModalRouteDetails
                      { gtfsId = Just $ T.pack route.gtfsId,
                        longName = fmap T.pack route.longName,
                        shortName = fmap T.pack route.shortName,
                        color = fmap T.pack route.color
                      }
                  ]
                Nothing -> []
              (fromStopCode, fromStopGtfsId) = case otpLeg'.from.stop of
                Just x -> (x.code, Just x.gtfsId)
                Nothing -> (Nothing, Nothing)
              fromStopDetails' =
                if mode == Walk
                  then Nothing
                  else
                    Just
                      MultiModalStopDetails
                        { stopCode = fmap T.pack fromStopCode,
                          name = startLocName,
                          gtfsId = fmap T.pack fromStopGtfsId
                        }
              (toStopCode, toStopGtfsId) = case otpLeg'.to.stop of
                Just x -> (x.code, Just x.gtfsId)
                Nothing -> (Nothing, Nothing)
              toStopDetails' =
                if mode == Walk
                  then Nothing
                  else
                    Just
                      MultiModalStopDetails
                        { stopCode = fmap T.pack toStopCode,
                          name = endLocName,
                          gtfsId = fmap T.pack toStopGtfsId
                        }
              genericAgency = case routeAgency of
                Nothing -> Nothing
                Just ag ->
                  Just $
                    MultiModalAgency
                      { gtfsId = (\x -> Just $ T.pack x.gtfsId) =<< ag.agency,
                        name = maybe "" (\x -> T.pack x.name) ag.agency
                      }

              -- Update the frequency map only if longName exists
              newFreqMap = case (maybeLongName, fromArrivalTime') of
                (Just longName, Just time) ->
                  let key = T.pack longName
                   in HM.insertWith (++) key [time] updatedFreqMap
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
                    toDepartureTime = toDepartureTime',
                    frequency = Nothing
                  }
           in (leg : genericLegs, genericDistance + distance, newFreqMap)

    -- Update frequency of each leg in a route using the frequencyMap
    updateRouteFrequency :: HM.HashMap T.Text [UTCTime] -> MultiModalRoute -> MultiModalRoute
    updateRouteFrequency freqMap route =
      let updatedLegs = map (updateLegFrequency freqMap) route.legs
       in route {legs = updatedLegs}

    -- Update frequency for a single leg
    updateLegFrequency :: HM.HashMap T.Text [UTCTime] -> MultiModalLeg -> MultiModalLeg
    updateLegFrequency freqMap leg =
      case listToMaybe $ mapMaybe longName (routeDetails leg) of --case routeDetails leg >>= longName of
        Just longNameText ->
          let key = longNameText
              timestamps = sort $ HM.lookupDefault [] key freqMap
              frequency = case timestamps of
                (t1 : t2 : _) -> Just $ Time.Seconds $ round $ diffUTCTime t2 t1
                _ -> Nothing
           in leg {frequency = frequency}
        Nothing -> leg

    -- Function to get the sequence combination for a route
    getSequenceCombination :: MultiModalRoute -> T.Text
    getSequenceCombination route =
      let sequenceCombination = mapMaybe (listToMaybe . mapMaybe longName . routeDetails) (route.legs)
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
