{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kernel.Types.Common
  ( HighPrecMoney (..),
    Currency (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Prelude

newtype HighPrecMoney = HighPrecMoney
  { getHighPrecMoney :: Rational
  }
  deriving stock (Generic)
  deriving newtype (Num, Real, Fractional, RealFrac, Ord, Eq)

instance Show HighPrecMoney where
  show = show @Double . realToFrac

instance Read HighPrecMoney where
  readsPrec d s = do
    (v, s1) :: (Double, String) <- readsPrec d s
    return (realToFrac v, s1)

instance ToJSON HighPrecMoney where
  toJSON = A.toJSON @Double . realToFrac

instance FromJSON HighPrecMoney where
  parseJSON = fmap realToFrac . A.parseJSON @Double

data Currency = INR | USD | EUR
  deriving stock (Generic, Show, Read, Eq, Ord)

instance ToJSON Currency

instance FromJSON Currency
