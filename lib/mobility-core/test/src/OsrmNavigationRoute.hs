{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module OsrmNavigationRoute where

import EulerHS.Prelude
import Kernel.External.Maps.OSRM.RoadsClient
  ( NavigationCoordinates (..),
    guideFallbackReasonFromBody,
    guidePointSnapRadiusMeters,
    navigationCoordinates,
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
      doesNotFallBackOnOtherFailures
    ]
