{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Types.Centesimal (Centesimal (..)) where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Utils.GenericPretty
import Control.Lens.Operators
import Data.Aeson
import Data.Fixed (Centi)
import Data.OpenApi

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
