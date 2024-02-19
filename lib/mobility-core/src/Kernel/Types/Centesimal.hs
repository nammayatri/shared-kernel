{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.Types.Centesimal (Centesimal (..), toCentesimal) where

import Control.Lens.Operators
import Data.Aeson
import Data.Fixed (Centi, Fixed (MkFixed))
import Data.OpenApi
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty

newtype Centesimal = Centesimal {getCenti :: Centi}
  deriving (Eq, Ord, Generic, Enum)
  deriving newtype (Num, Real, RealFrac, ToJSON, Read, Show, PersistField, PersistFieldSql)
  deriving anyclass (PrettyShow)

instance FromJSON Centesimal where
  parseJSON = withScientific "Centesimal" (pure . realToFrac)

instance Fractional Centesimal where
  (Centesimal a) / (Centesimal b) = Centesimal $ a / b
  recip (Centesimal a) = Centesimal $ recip a
  fromRational rat = Centesimal . fromRational $ roundToPowerOfTen (-2) rat

maxPrecision :: Int
maxPrecision = 30

message :: Text
message =
  "Maximum allowed precision (total number of digits) is "
    <> show maxPrecision

toCentesimal :: Int -> Centesimal
toCentesimal = Centesimal . MkFixed . toInteger

instance ToSchema Centesimal where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "Centesimal") $
        aSchema
          & description
            ?~ "Monetary amount in a string representation \
               \with an optional leading \"-\" for negative numbers. \
               \Integer and fractional parts are separated with a dot."
              <> message
              <> " String format is used to prevent loss of precision."
          & format ?~ "[-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?"
