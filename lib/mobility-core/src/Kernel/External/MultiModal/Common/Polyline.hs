module Kernel.External.MultiModal.Common.Polyline (encode, decode) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Maps.Google.MapsClient.Types as GT
import Kernel.External.Maps.Google.PolyLinePoints (oneCoordEnc, stringToCoords)
import Kernel.Prelude

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
