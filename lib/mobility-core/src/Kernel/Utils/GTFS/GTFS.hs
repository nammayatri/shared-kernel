module Kernel.Utils.GTFS.GTFS where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Csv
import qualified Data.Vector as V
import Kernel.Prelude
import Kernel.Utils.GTFS.Types
import System.Directory
import qualified System.FilePath as Path
import System.IO.Unsafe (unsafeInterleaveIO)

parseFile :: (FromNamedRecord a) => FilePath -> IO [a]
parseFile p = do
  ex <- doesFileExist p
  if not ex then return [] else go
  where
    go = do
      x <- decodeByName <$> BSL8.readFile p
      case x of
        Left _ -> error "parse failure"
        Right (_header, ys) -> return (V.toList ys)

parseFeed :: FilePath -> IO Feed
parseFeed d =
  Feed <$> f "agency.txt" <*> f "stops.txt" <*> f "routes.txt"
    <*> f "trips.txt"
    <*> f "stop_times.txt"
    <*> f "calendar.txt"
    <*> f "calendar_dates.txt"
    <*> f "fare_attributes.txt"
    <*> f "fare_rules.txt"
    <*> f "shapes.txt"
    <*> f "frequencies.txt"
    <*> f "transfers.txt"
    <*> f "feed_info.txt"
  where
    f :: (FromNamedRecord a) => String -> IO [a]
    f x = unsafeInterleaveIO . parseFile $ Path.combine d x
