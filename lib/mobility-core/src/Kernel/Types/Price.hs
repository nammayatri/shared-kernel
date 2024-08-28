{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=identities #-}

module Kernel.Types.Price where

import Data.Aeson
import Data.OpenApi hiding (value)
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Persist.Class
import Database.Persist.Sql
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Kernel.Prelude as KP
import qualified Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.FromField
import Kernel.Types.Logging
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.Error.Throwing as E (throwError)
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant
import Text.Show (Show (..))

-- To be deprecated
newtype Money = Money
  { getMoney :: Int
  }
  deriving stock (Generic)
  deriving newtype (Show, Read, PrettyShow, Enum, Eq, Ord, Num, Real, Integral, PersistField, PersistFieldSql, ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Money where
  sqlValueSyntax = sqlValueSyntax . getMoney

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

instance FromField Money where
  fromField = fromFieldJSON

newtype HighPrecMoney = HighPrecMoney
  { getHighPrecMoney :: Rational
  }
  deriving stock (Generic)
  deriving newtype (Num, FromDhall, Real, Fractional, RealFrac, Ord, Eq, Enum, PrettyShow, PersistField, PersistFieldSql)

instance Show HighPrecMoney where
  show = Text.Show.show @Double . realToFrac

instance Read HighPrecMoney where
  readsPrec d s = do
    (dobuleVal, s1) :: (Double, String) <- readsPrec d s
    return (realToFrac dobuleVal, s1)

instance ToJSON HighPrecMoney where
  toJSON = toJSON @Double . realToFrac

instance FromJSON HighPrecMoney where
  parseJSON = fmap realToFrac . parseJSON @Double

instance ToParamSchema HighPrecMoney where
  toParamSchema _ = toParamSchema (Proxy @Double)

instance FromField HighPrecMoney where
  fromField f mbValue = HighPrecMoney <$> fromFieldDefault f mbValue

instance HasSqlValueSyntax be Rational => HasSqlValueSyntax be HighPrecMoney where
  sqlValueSyntax = sqlValueSyntax . getHighPrecMoney

instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Rational where
  sqlValueSyntax = sqlValueSyntax . (fromRational :: Rational -> Double)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

instance FromBackendRow Postgres HighPrecMoney

instance ToSchema HighPrecMoney where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Double)
    return $ NamedSchema (Just "HighPrecMoney") aSchema

$(mkHttpInstancesForEnum ''HighPrecMoney)

data Currency = INR | USD | EUR
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable Currency

$(mkHttpInstancesForEnum ''Currency)

-- cycle imports

-- $(mkBeamInstancesForEnum ''Currency)

instance FromField Currency where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Currency where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Currency

instance FromBackendRow Postgres Currency

data Price = Price
  { amountInt :: Money, -- To be deprecated
    amount :: HighPrecMoney,
    currency :: Currency
  }
  deriving anyclass (ToJSON, FromJSON, ToSchema)
  deriving stock (Generic, Show, Read)
  deriving (PrettyShow) via Showable Price

mkPrice :: Maybe Currency -> HighPrecMoney -> Price
mkPrice mbCurrency amount =
  Price
    { amountInt = roundToIntegral amount,
      amount,
      currency = fromMaybe INR mbCurrency
    }

mkPriceWithDefault :: Maybe HighPrecMoney -> Maybe Currency -> Money -> Price -- FIXME Currency
mkPriceWithDefault mbAmount mbCurrency defAmount =
  Price
    { amountInt = maybe defAmount roundToIntegral mbAmount,
      amount = mkAmountWithDefault mbAmount defAmount,
      currency = fromMaybe INR mbCurrency
    }

mkAmountWithDefault :: Maybe HighPrecMoney -> Money -> HighPrecMoney
mkAmountWithDefault mbAmount defAmount = fromMaybe (HighPrecMoney $ toRational defAmount) mbAmount

mkPriceFromAPIEntity :: PriceAPIEntity -> Price
mkPriceFromAPIEntity priceAPIEntity = mkPrice (Just priceAPIEntity.currency) priceAPIEntity.amount

-- To be deprecated
mkPriceFromMoney :: Maybe Currency -> Money -> Price
mkPriceFromMoney = mkPriceWithDefault Nothing

-- To be deprecated when remove Money
data PriceAPIEntity = PriceAPIEntity
  { amount :: HighPrecMoney,
    currency :: Currency
  }
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

mkPriceAPIEntity :: Price -> PriceAPIEntity
mkPriceAPIEntity Price {..} = PriceAPIEntity {..}

withCurrencyChecking ::
  (MonadThrow m, Log m) =>
  Price ->
  Price ->
  (HighPrecMoney -> HighPrecMoney -> a) ->
  m a
withCurrencyChecking p1 p2 func = do
  if p1.currency == p2.currency
    then pure $ func p1.amount p2.amount
    else
      E.throwError $
        InternalError $ "Trying to make operation for prices with different currencies: " <> KP.show p1 <> "; " <> KP.show p2

withCurrencyChecking3 ::
  (MonadThrow m, Log m) =>
  Price ->
  Price ->
  Price ->
  (HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> a) ->
  m a
withCurrencyChecking3 p1 p2 p3 func = do
  if p1.currency == p2.currency && p2.currency == p3.currency
    then pure $ func p1.amount p2.amount p3.amount
    else
      E.throwError $
        InternalError $ "Trying to make operation for prices with different currencies: " <> KP.show p1 <> "; " <> KP.show p2 <> "; " <> KP.show p3

-- | Do not use on endless lists
withCurrencyCheckingList ::
  (MonadThrow m, Log m) =>
  [Price] ->
  (Maybe Currency -> [HighPrecMoney] -> a) ->
  m a
withCurrencyCheckingList [] func = pure $ func Nothing []
withCurrencyCheckingList ps@(p1 : _) func = do
  if all (\p -> p.currency == p1.currency) ps
    then pure $ func (Just p1.currency) (ps <&> (.amount))
    else
      E.throwError $
        InternalError $ "Trying to make operation for prices with different currencies: " <> KP.show ps

addPrice :: (MonadThrow m, Log m) => Price -> Price -> m Price
addPrice p1 p2 = mkPrice (Just p1.currency) <$> withCurrencyChecking p1 p2 (+)

subtractPrice :: (MonadThrow m, Log m) => Price -> Price -> m Price
subtractPrice p1 p2 = mkPrice (Just p1.currency) <$> withCurrencyChecking p1 p2 (-)

modifyPrice :: Price -> (HighPrecMoney -> HighPrecMoney) -> Price
modifyPrice price func = mkPrice (Just price.currency) (func price.amount)

highPrecMoneyFromText :: Text -> Maybe HighPrecMoney
highPrecMoneyFromText txt = do
  DecimalValue.DecimalValue rational <- DecimalValue.valueFromString txt
  pure $ HighPrecMoney rational

highPrecMoneyToText :: HighPrecMoney -> Text
highPrecMoneyToText = DecimalValue.valueToString . DecimalValue.DecimalValue . getHighPrecMoney

toHighPrecMoney :: Real a => a -> HighPrecMoney
toHighPrecMoney = HighPrecMoney . toRational

showPriceWithRounding :: Price -> Text
showPriceWithRounding price = case getAccuracy price.currency of
  0 -> KP.show @Text @Integer (round price.amount) <> " " <> KP.show price.currency
  accuracy -> KP.show @Text @Double (fromIntegral (round (price.amount.getHighPrecMoney * 10 ^ accuracy) :: Integer) / 10 ^ accuracy) <> " " <> KP.show price.currency

getAccuracy :: Currency -> Int
getAccuracy INR = 0
getAccuracy USD = 2
getAccuracy EUR = 2

showPriceWithRoundingWithoutCurrency :: Price -> Text
showPriceWithRoundingWithoutCurrency price = case getAccuracy price.currency of
  0 -> KP.show @Text @Integer (round price.amount)
  accuracy -> KP.show @Text @Double (fromIntegral (round (price.amount.getHighPrecMoney * 10 ^ accuracy) :: Integer) / 10 ^ accuracy)
