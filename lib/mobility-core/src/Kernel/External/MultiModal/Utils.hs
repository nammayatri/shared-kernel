module Kernel.External.MultiModal.Utils
  ( convertGoogleToGeneric,
    convertOTPToGeneric,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
import qualified Kernel.External.Maps.OpenTripPlanner.Types as OTP
import qualified Kernel.External.MultiModal.Common.Polyline as GP
import Kernel.External.MultiModal.Types
import Kernel.Prelude (read)
import Kernel.Utils.Time (millisecondsToUTC, parseISO8601UTC)

extractDuration :: T.Text -> Int
extractDuration t = read (filter Char.isDigit (T.unpack t)) :: Int

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
          routeDistance = gRoute.distanceMeters
          gLegs = gRoute.legs
          routeLegs = adjustWalkingLegs $ mergeWalkingLegs $ foldr accumulateLegs [] gLegs
       in MultiModalRoute
            { distance = fromIntegral routeDistance,
              duration = routeDuration,
              legs = routeLegs
            } :
          genericRoutes
    accumulateLegs :: GT.LegV2 -> [MultiModalLeg] -> [MultiModalLeg]
    accumulateLegs gLeg genericLegs =
      let gSteps = gLeg.steps
       in genericLegs ++ foldr accumulateSteps [] gSteps
    accumulateSteps :: GT.StepV2 -> [MultiModalLeg] -> [MultiModalLeg]
    accumulateSteps gStep genericLegs =
      let stepDistance = fromIntegral gStep.distanceMeters
          stepDuration = extractDuration gStep.staticDuration
          stepPolyline = GT.encodedPolyline gStep.polyline
          (stepTravelMode, genericAgency, fromStopDetails', toStopDetails', fromArrivalTime', fromDepartureTime', toArrivalTime', toDepartureTime') = case gStep.travelMode of
            GT.TRANSIT ->
              case gStep.transitDetails of
                Just details ->
                  let travelM = T.unpack details.transitLine.vehicle._type
                      gAgency = safeHead details.transitLine.agencies
                      eName = details.stopDetails.arrivalStop.name
                      sName = details.stopDetails.departureStop.name
                      fDepartureTime = parseISO8601UTC $ T.unpack details.stopDetails.departureTime
                      tArrivalTime = parseISO8601UTC $ T.unpack details.stopDetails.arrivalTime
                      fromDetails =
                        MultiModalStopDetails
                          { stopCode = Nothing,
                            name = Just sName
                          }
                      toDetails =
                        MultiModalStopDetails
                          { stopCode = Nothing,
                            name = Just eName
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
                Nothing -> ("TRANSIT", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            val -> (show val, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
          (startLocationLat, startLocationLng) = (gStep.startLocation.latLng.latitude, gStep.startLocation.latLng.longitude)
          (endLocationLat, endLocationLng) = (gStep.endLocation.latLng.latitude, gStep.endLocation.latLng.longitude)
       in MultiModalLeg
            { distance = stepDistance,
              duration = fromIntegral stepDuration,
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
              toDepartureTime = toDepartureTime'
            } :
          genericLegs
    mergeWalkingLegs :: [MultiModalLeg] -> [MultiModalLeg]
    mergeWalkingLegs [] = []
    mergeWalkingLegs (leg : legs) = mergeLeg leg legs
      where
        mergeLeg currentLeg [] = [currentLeg]
        mergeLeg currentLeg (nextLeg : restLegs)
          | currentLeg.mode == "WALK" && nextLeg.mode == "WALK" =
            mergeLeg (mergeTwoLegs currentLeg nextLeg) restLegs
          | otherwise = currentLeg : mergeLeg nextLeg restLegs

    mergeTwoLegs :: MultiModalLeg -> MultiModalLeg -> MultiModalLeg
    mergeTwoLegs leg1 leg2 =
      let leg1Start = leg1.startLocation
          leg2Start = leg2.startLocation
          leg2End = leg2.endLocation
          encodedPolylineText = GP.encode [leg1Start.latLng, leg2Start.latLng, leg2End.latLng]
       in MultiModalLeg
            { distance = leg1.distance + leg2.distance,
              duration = leg1.duration + leg2.duration,
              polyline = GT.Polyline {encodedPolyline = encodedPolylineText},
              mode = "WALK",
              startLocation = leg1Start,
              endLocation = leg2End,
              fromStopDetails = leg1.fromStopDetails,
              toStopDetails = leg2.toStopDetails,
              agency = Nothing,
              fromArrivalTime = leg1.fromArrivalTime,
              fromDepartureTime = leg1.fromDepartureTime,
              toArrivalTime = leg2.toArrivalTime,
              toDepartureTime = leg2.toDepartureTime
            }
    adjustWalkingLegs :: [MultiModalLeg] -> [MultiModalLeg]
    adjustWalkingLegs [] = []
    adjustWalkingLegs [leg] = [leg]
    adjustWalkingLegs (leg1 : leg2 : rest) =
      let adjustedLeg1 =
            if leg1.mode == "WALK" && leg2.mode /= "WALK"
              then leg1{toStopDetails = leg2.fromStopDetails, toDepartureTime = leg2.fromDepartureTime, toArrivalTime = leg2.fromArrivalTime}
              else leg1
          adjustedLeg2 =
            if leg2.mode == "WALK" && leg1.mode /= "WALK"
              then leg2{fromStopDetails = leg1.toStopDetails, fromDepartureTime = leg1.toDepartureTime, fromArrivalTime = leg1.toArrivalTime}
              else leg2
       in adjustedLeg1 : adjustWalkingLegs (adjustedLeg2 : rest)

convertOTPToGeneric :: OTP.OTPPlan -> MultiModalResponse
convertOTPToGeneric otpResponse =
  let itineraries = otpResponse.plan.itineraries
      genericRoutes = foldr accumulateItineraries [] itineraries
   in MultiModalResponse
        { routes = genericRoutes
        }
  where
    accumulateItineraries :: Maybe OTP.OTPPlanPlanItineraries -> [MultiModalRoute] -> [MultiModalRoute]
    accumulateItineraries itinerary genericRoutes =
      case itinerary of
        Nothing -> genericRoutes
        Just itinerary' ->
          let duration = fromMaybe 0.0 itinerary'.duration
              (legs, distance) = foldr accumulateLegs ([], 0.0) itinerary'.legs
              route =
                MultiModalRoute
                  { duration = round duration,
                    distance = distance,
                    legs = legs
                  }
           in route : genericRoutes
    accumulateLegs :: Maybe OTP.OTPPlanPlanItinerariesLegs -> ([MultiModalLeg], Double) -> ([MultiModalLeg], Double)
    accumulateLegs otpLeg (genericLegs, genericDistance) =
      case otpLeg of
        Nothing -> (genericLegs, genericDistance)
        Just otpLeg' ->
          let distance = fromMaybe 0.0 otpLeg'.distance
              duration = fromMaybe 0.0 otpLeg'.duration
              mode = fromMaybe "TRANSIT" otpLeg'.mode
              startLocName = fmap T.pack $ if fromMaybe "" otpLeg'.from.name == "Origin" then Nothing else otpLeg'.from.name
              endLocName = fmap T.pack $ if fromMaybe "" otpLeg'.to.name == "Destination" then Nothing else otpLeg'.to.name
              encodedPolylineText = T.pack $ maybe "" (\x -> fromMaybe "" x.points) otpLeg'.legGeometry
              (startLat, startLng) = (otpLeg'.from.lat, otpLeg'.from.lon)
              (endLat, endLng) = (otpLeg'.to.lat, otpLeg'.to.lon)
              routeAgency = otpLeg'.route
              fromArrivalTime' = Just $ millisecondsToUTC $ round otpLeg'.from.arrivalTime
              fromDepartureTime' = Just $ millisecondsToUTC $ round otpLeg'.from.departureTime
              toArrivalTime' = Just $ millisecondsToUTC $ round otpLeg'.to.arrivalTime
              toDepartureTime' = Just $ millisecondsToUTC $ round otpLeg'.to.departureTime
              fromStopCode = case otpLeg'.from.stop of
                Just x -> x.code
                Nothing -> Nothing
              fromStopDetails' =
                if mode == "WALK"
                  then Nothing
                  else
                    Just
                      MultiModalStopDetails
                        { stopCode = fromStopCode,
                          name = startLocName
                        }
              toStopCode = case otpLeg'.to.stop of
                Just x -> x.code
                Nothing -> Nothing
              toStopDetails' =
                if mode == "WALK"
                  then Nothing
                  else
                    Just
                      MultiModalStopDetails
                        { stopCode = toStopCode,
                          name = endLocName
                        }
              genericAgency = case routeAgency of
                Nothing -> Nothing
                Just ag ->
                  Just $
                    MultiModalAgency
                      { gtfsId = (\x -> Just $ T.pack x.gtfsId) =<< ag.agency,
                        name = maybe "" (\x -> T.pack x.name) ag.agency
                      }
              leg =
                MultiModalLeg
                  { distance = distance,
                    duration = duration,
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
           in (leg : genericLegs, genericDistance + distance)
