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
{-# LANGUAGE UndecidableInstances #-}

-- Functions rationalToString, validate
-- are only exported for testing purposes.
module Kernel.Types.Beckn.DecimalValue
  ( DecimalValue (..),
    valueToString,
    valueFromString,
  )
where

import Control.Lens.Operators
import Data.Char
import Data.OpenApi hiding (Example, value)
import Data.Proxy
import qualified Data.Ratio as R
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty (PrettyShow)
import qualified Money as M

-- | A type for decimal values based on Rational.
-- Uses "integer.fractional" format for serialization / deserialization.
-- Maximum precision (total number of digits) is defined in this module.
-- Note: serialization of numbers whose integer part has more digits than
-- the maximum precision will fail with an error.
-- Functions / and recip will fail with an error if the denominator is zero.
newtype DecimalValue = DecimalValue Rational
  deriving (Eq, Ord, Show, Read, Generic)
  deriving newtype (Num, Real, Fractional, RealFrac)
  deriving anyclass (PrettyShow)

instance Example DecimalValue where
  example = DecimalValue 10

maxPrecisionWord8 :: Word8
maxPrecisionWord8 = 30

maxPrecision :: Int
maxPrecision = fromIntegral maxPrecisionWord8

message :: Text
message =
  "Maximum allowed precision (total number of digits) is "
    <> show maxPrecision

-- Functions rationalToString and valueToString should only be used
-- for serialization. They don't perform any proper rounding and simply
-- stop generating digits when at the maximum precision.
-- Note: rationalToString will return Nothing if the integer
-- part of the number exceeds the precision (total number of digits).
rationalToString :: Int -> Rational -> Maybe String
rationalToString precision rational
  | length intPart > precision = Nothing
  | otherwise = Just result
  where
    result =
      if fracPrecision <= 0 || null fracPart
        then intPart
        else intPart <> "." <> take fracPrecision fracPart
    rNumerator = R.numerator rational
    rDenominator = R.denominator rational
    sign = if rNumerator < 0 then "-" else ""
    intPart = sign <> show quotient
    fracPrecision = precision - length intPart
    fracPart = expand remainder
    (quotient, remainder) = abs rNumerator `quotRem` rDenominator
    expand currentRemainder
      | currentRemainder == 0 = ""
      | otherwise = show digit <> expand nextRemainder
      where
        (digit, nextRemainder) = (10 * currentRemainder) `quotRem` rDenominator

-- Note: valueToString will fail with an error if the integer
-- part of the number exceeds the precision (total number of digits).
valueToString :: DecimalValue -> Text
valueToString value =
  maybe
    (error ("Cannot convert " <> show value <> " to a string. " <> message))
    T.pack
    (rationalToString maxPrecision (toRational value))

valueFromString :: Text -> Maybe DecimalValue
valueFromString valueString =
  DecimalValue . toRational <$> M.denseFromDecimal decimalConf valueString
  where
    -- The exact value passed in DecimalConf.decimalConf_digits to
    -- denseFromDecimal does not matter, but it should be large enough to
    -- make sure there is no precision loss.
    decimalConf =
      M.defaultDecimalConf
        { M.decimalConf_digits = maxPrecisionWord8
        }

validate :: Int -> Text -> Bool
validate precision valueString =
  T.length valueString <= maxPossibleLength
    && countDigits valueString <= precision
  where
    -- Combined length of "-" and "."
    maxNonDigitLength = 2
    maxPossibleLength = T.length valueString + maxNonDigitLength
    countDigits = T.length . T.filter isDigit

-- Note: toJSON will fail with an error if the integer
-- part of the number exceeds the precision (total number of digits).
instance ToJSON DecimalValue where
  toJSON = toJSON . valueToString

instance FromJSON DecimalValue where
  parseJSON value = do
    valueString <- parseJSON value
    unless (validate maxPrecision valueString) $ failText message
    maybe (parseError valueString) pure $ valueFromString valueString
    where
      parseError valueString =
        failText $ "Cannot parse " <> valueString <> " as a DecimalValue."
      failText = fail . T.unpack

instance ToSchema DecimalValue where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "DecimalValue") $
        aSchema
          & description
            ?~ "Decimal value in a string representation \
               \with an optional leading \"-\" for negative numbers. \
               \Integer and fractional parts are separated with a dot."
              <> message
              <> " String format is used to prevent loss of precision."
          & format ?~ "[-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?"
