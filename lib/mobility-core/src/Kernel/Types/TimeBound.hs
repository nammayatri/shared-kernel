{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Kernel.Types.TimeBound
  ( BoundedPeaks (..),
    TimeBound (..),
    findBoundedDomain,
    timeBoundsOverlap,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as Text
import Data.Time
import Data.Time.Calendar.WeekDate
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty
import qualified Text.Show

data BoundedPeaks = BoundedPeaks
  { monday :: [(TimeOfDay, TimeOfDay)],
    tuesday :: [(TimeOfDay, TimeOfDay)],
    wednesday :: [(TimeOfDay, TimeOfDay)],
    thursday :: [(TimeOfDay, TimeOfDay)],
    friday :: [(TimeOfDay, TimeOfDay)],
    saturday :: [(TimeOfDay, TimeOfDay)],
    sunday :: [(TimeOfDay, TimeOfDay)]
  }
  deriving (Eq, Ord, Generic, Show, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable BoundedPeaks

data TimeBound
  = BoundedByWeekday BoundedPeaks
  | BoundedByDay [(Day, [(TimeOfDay, TimeOfDay)])]
  | Unbounded
  deriving (Eq, Ord, Generic)
  deriving anyclass (ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TimeBound

instance Show TimeBound where
  show Unbounded = "Unbounded"
  show (BoundedByWeekday peaks) = show peaks
  show (BoundedByDay days) = show days

instance Read TimeBound where
  readsPrec _ str
    | str == "Unbounded" = [(Unbounded, "")]
    | otherwise =
      case (readMaybe str :: Maybe BoundedPeaks) of
        Just bound -> [(BoundedByWeekday bound, "")]
        Nothing ->
          case (readMaybe str :: Maybe [(Day, [(TimeOfDay, TimeOfDay)])]) of
            Just bound -> [(BoundedByDay bound, "")]
            Nothing -> [(Unbounded, "")]

instance FromJSON TimeBound where
  parseJSON (String val) = do
    case (readMaybe (Text.unpack val) :: Maybe TimeBound) of
      Just bound -> pure bound
      Nothing -> fail "Invalid TimeBound"
  parseJSON other = genericParseJSON defaultOptions other

$(mkBeamInstancesForEnum ''TimeBound)

findBoundedDomain :: (HasField "timeBounds" domain TimeBound) => [domain] -> UTCTime -> [domain]
findBoundedDomain domains localTime = do
  let currTimeOfDay = utcTimeToDiffTime localTime
      currentDay = utctDay localTime
      (_, _, currentDayOfWeek) = toWeekDate currentDay
  let (domainsBoundedByWeekday, domainsBoundedByDay) =
        foldl
          ( \acc@(domainsBoundedByWeekday_, domainsBoundedByDay_) domain ->
              case domain.timeBounds of
                BoundedByWeekday timeBounds ->
                  if isWithin currTimeOfDay (handleTwentyFourHourClockCycle $ getPeaksForCurrentDay currentDayOfWeek timeBounds)
                    then (domainsBoundedByWeekday_ <> [domain], domainsBoundedByDay_)
                    else acc
                BoundedByDay days ->
                  if maybe False (isWithin currTimeOfDay) (handleTwentyFourHourClockCycle <$> snd <$> find (\(day, _) -> day == currentDay) days)
                    then (domainsBoundedByWeekday_, domainsBoundedByDay_ <> [domain])
                    else acc
                Unbounded -> acc
          )
          ([], [])
          domains
  domainsBoundedByWeekday <|> domainsBoundedByDay
  where
    isWithin _ [] = False
    isWithin currTime [(startTime, endTime)] = currTime > timeOfDayToTime startTime && currTime < timeOfDayToTime endTime
    isWithin currTime ((startTime, endTime) : xs) = (currTime > timeOfDayToTime startTime && currTime < timeOfDayToTime endTime) || isWithin currTime xs

    handleTwentyFourHourClockCycle =
      foldl
        ( \timeBounds (startTime, endTime) ->
            if endTime < startTime
              then timeBounds <> [(startTime, TimeOfDay 23 59 59), (TimeOfDay 00 00 00, endTime)]
              else timeBounds <> [(startTime, endTime)]
        )
        []

    getPeaksForCurrentDay currentDayOfWeek peaks =
      case currentDayOfWeek of
        1 -> peaks.monday
        2 -> peaks.tuesday
        3 -> peaks.wednesday
        4 -> peaks.thursday
        5 -> peaks.friday
        6 -> peaks.saturday
        7 -> peaks.sunday
        _ -> peaks.monday -- This case should never come.

-- A helper function to check if two time intervals overlap
overlaps :: (TimeOfDay, TimeOfDay) -> (TimeOfDay, TimeOfDay) -> Bool
overlaps (start1, end1) (start2, end2) = not (end1 <= start2 || end2 <= start1)

-- A function to check if any time intervals overlap in a list
anyOverlap :: [(TimeOfDay, TimeOfDay)] -> [(TimeOfDay, TimeOfDay)] -> Bool
anyOverlap xs ys = any (\x -> any (overlaps x) ys) xs

-- A function to check if two TimeBounds overlap
timeBoundsOverlap :: TimeBound -> TimeBound -> Bool
timeBoundsOverlap Unbounded _ = True
timeBoundsOverlap _ Unbounded = True
timeBoundsOverlap (BoundedByWeekday bp1) (BoundedByWeekday bp2) =
  anyOverlap (monday bp1) (monday bp2)
    || anyOverlap (tuesday bp1) (tuesday bp2)
    || anyOverlap (wednesday bp1) (wednesday bp2)
    || anyOverlap (thursday bp1) (thursday bp2)
    || anyOverlap (friday bp1) (friday bp2)
    || anyOverlap (saturday bp1) (saturday bp2)
    || anyOverlap (sunday bp1) (sunday bp2)
timeBoundsOverlap (BoundedByDay bd1) (BoundedByDay bd2) =
  any (\(d1, ts1) -> any (\(d2, ts2) -> d1 == d2 && anyOverlap ts1 ts2) bd2) bd1
timeBoundsOverlap _ _ = False
