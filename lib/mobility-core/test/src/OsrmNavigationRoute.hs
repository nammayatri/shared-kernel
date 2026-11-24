{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module OsrmNavigationRoute where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import EulerHS.Prelude
import Kernel.External.Maps.OSRM.RoadsClient
  ( NavigationCoordinates (..),
    OsrmNavigationRouteResp,
    guideFallbackReasonFromBody,
    guidePointSnapRadiusMeters,
    navigationCoordinates,
    navigationRouteResp,
  )
import Kernel.External.Maps.Types (LatLong (..))
import Test.Tasty
import Test.Tasty.HUnit

origin, via1, via2, stop1, destination :: LatLong
origin = LatLong 12.94225 77.62204
via1 = LatLong 12.94253 77.62264
via2 = LatLong 12.94389 77.62110
stop1 = LatLong 12.94400 77.62000
destination = LatLong 12.94481 77.61974

withoutGuidePoints :: TestTree
withoutGuidePoints =
  testCase "without guide points every coordinate is a stop and no extra parameters are sent" $ do
    let coordinates = navigationCoordinates origin [] [destination] Nothing
    routeCoordinates coordinates @?= [origin, destination]
    stopIndices coordinates @?= Nothing
    radiusValues coordinates @?= Nothing
    bearingValues coordinates @?= Nothing

guidePointsArePassThrough :: TestTree
guidePointsArePassThrough =
  testCase "guide points come between the origin and the destination and are not stops" $ do
    let coordinates = navigationCoordinates origin [via1, via2] [destination] Nothing
    routeCoordinates coordinates @?= [origin, via1, via2, destination]
    stopIndices coordinates @?= Just "0;3"

guidePointsBeforeEveryStop :: TestTree
guidePointsBeforeEveryStop =
  testCase "every waypoint after the guide points stays a stop" $ do
    let coordinates = navigationCoordinates origin [via1, via2] [stop1, destination] Nothing
    routeCoordinates coordinates @?= [origin, via1, via2, stop1, destination]
    stopIndices coordinates @?= Just "0;3;4"

guidePointsHaveBoundedRadius :: TestTree
guidePointsHaveBoundedRadius =
  testCase "only the guide points have a bounded snap radius" $ do
    let coordinates = navigationCoordinates origin [via1, via2] [destination] Nothing
        radius = show guidePointSnapRadiusMeters
    radiusValues coordinates @?= Just ("unlimited;" <> radius <> ";" <> radius <> ";unlimited")

headingCoversEveryCoordinate :: TestTree
headingCoversEveryCoordinate =
  testCase "the heading is for the origin only, with an empty entry for each other coordinate" $ do
    bearingValues (navigationCoordinates origin [] [destination] (Just 90)) @?= Just "90,45;"
    bearingValues (navigationCoordinates origin [via1, via2] [destination] (Just 90)) @?= Just "90,45;;;"

headingWrapsAround :: TestTree
headingWrapsAround =
  testCase "a heading past 360 degrees wraps around" $
    bearingValues (navigationCoordinates origin [] [destination] (Just 365)) @?= Just "5,45;"

fallsBackWhenPointsDontFitTheRoads :: TestTree
fallsBackWhenPointsDontFitTheRoads =
  testCase "guide points are dropped when OSRM finds no road for one or cannot connect them" $ do
    guideFallbackReasonFromBody "{\"code\":\"NoSegment\",\"message\":\"Could not find a matching segment for coordinate 1\"}" @?= Just "NoSegment"
    guideFallbackReasonFromBody "{\"code\":\"NoRoute\",\"message\":\"Impossible route between points\"}" @?= Just "NoRoute"

doesNotFallBackOnOtherFailures :: TestTree
doesNotFallBackOnOtherFailures =
  testCase "other OSRM errors and unreadable bodies do not drop the guide points" $ do
    guideFallbackReasonFromBody "{\"code\":\"InvalidValue\",\"message\":\"Exclude flag combination is not supported.\"}" @?= Nothing
    guideFallbackReasonFromBody "<html>502 Bad Gateway</html>" @?= Nothing
    guideFallbackReasonFromBody "" @?= Nothing

-- | An OSRM route response with one step, carrying fields navigation doesn't read next to the ones it does.
osrmResponse :: [A.Value] -> A.Value
osrmResponse intersections =
  A.object
    [ "code" .= ("Ok" :: Text),
      "waypoints" .= [A.object ["hint" .= ("f8ydjf" :: Text), "location" .= startPoint]],
      "routes"
        .= [ A.object
               [ "geometry" .= stepGeometry,
                 "distance" .= (154.9 :: Double),
                 "duration" .= (25.3 :: Double),
                 "weight" .= (25.3 :: Double),
                 "weight_name" .= ("routability" :: Text),
                 "legs"
                   .= [ A.object
                          [ "distance" .= (154.9 :: Double),
                            "duration" .= (25.3 :: Double),
                            "weight" .= (25.3 :: Double),
                            "summary" .= ("Mahatma Gandhi Road" :: Text),
                            "annotation" .= annotation,
                            "steps"
                              .= [ A.object
                                     [ "geometry" .= stepGeometry,
                                       "distance" .= (154.9 :: Double),
                                       "duration" .= (25.3 :: Double),
                                       "weight" .= (25.3 :: Double),
                                       "mode" .= ("driving" :: Text),
                                       "driving_side" .= ("right" :: Text),
                                       "name" .= ("Mahatma Gandhi Road" :: Text),
                                       "maneuver" .= A.object (maneuver <> ["bearing_before" .= (0 :: Double)]),
                                       "intersections" .= intersections
                                     ]
                                 ]
                          ]
                      ]
               ]
           ]
    ]

-- | What the navigation route keeps of 'osrmResponse'.
trimmedResponse :: A.Value
trimmedResponse =
  A.object
    [ "code" .= ("Ok" :: Text),
      "tollsAvoided" .= False,
      "followsSearchRoute" .= True,
      "routes"
        .= [ A.object
               [ "geometry" .= stepGeometry,
                 "distance" .= (154.9 :: Double),
                 "duration" .= (25.3 :: Double),
                 "legs"
                   .= [ A.object
                          [ "distance" .= (154.9 :: Double),
                            "duration" .= (25.3 :: Double),
                            "annotation" .= annotation,
                            "steps"
                              .= [ A.object
                                     [ "geometry" .= stepGeometry,
                                       "distance" .= (154.9 :: Double),
                                       "duration" .= (25.3 :: Double),
                                       "name" .= ("Mahatma Gandhi Road" :: Text),
                                       "maneuver" .= A.object (maneuver <> ["bearing_before" .= (0 :: Double)]),
                                       "intersections" .= [A.object ["location" .= startPoint]]
                                     ]
                                 ]
                          ]
                      ]
               ]
           ]
    ]

startPoint :: [Double]
startPoint = [77.60658, 12.975523]

stepGeometry :: Text
stepGeometry = "ey}vWgjv_sCdL}t@fDoTVyA|@iG"

maneuver :: [(A.Key, A.Value)]
maneuver = ["location" .= startPoint, "bearing_after" .= (104 :: Double), "type" .= ("depart" :: Text), "modifier" .= ("left" :: Text)]

annotation :: A.Value
annotation =
  A.object
    [ "distance" .= [96.5, 58.4 :: Double],
      "duration" .= [14.5, 10.8 :: Double],
      "speed" .= [6.7, 5.4 :: Double],
      "nodes" .= [245618233, 1336721086, 9124871110 :: Integer]
    ]

fullIntersection :: A.Value
fullIntersection = A.object ["location" .= startPoint, "bearings" .= [104 :: Int], "entry" .= [True], "out" .= (0 :: Int)]

keepsOnlyNavigationFields :: TestTree
keepsOnlyNavigationFields =
  testCase "the route keeps only the fields navigation reads, under OSRM's names, plus the two flags" $ do
    let decoded = A.decode (A.encode (osrmResponse [fullIntersection])) :: Maybe OsrmNavigationRouteResp
    A.toJSON . navigationRouteResp False True <$> decoded @?= Just trimmedResponse

refusesStepWithoutIntersections :: TestTree
refusesStepWithoutIntersections =
  testCase "a step without intersections is refused, as the navigation engine would crash on it" $
    (A.decode (A.encode (osrmResponse [])) :: Maybe OsrmNavigationRouteResp) @?= Nothing

osrmNavigationRouteTests :: TestTree
osrmNavigationRouteTests =
  testGroup
    "OSRM navigation route"
    [ withoutGuidePoints,
      guidePointsArePassThrough,
      guidePointsBeforeEveryStop,
      guidePointsHaveBoundedRadius,
      headingCoversEveryCoordinate,
      headingWrapsAround,
      fallsBackWhenPointsDontFitTheRoads,
      doesNotFallBackOnOtherFailures,
      keepsOnlyNavigationFields,
      refusesStepWithoutIntersections
    ]
