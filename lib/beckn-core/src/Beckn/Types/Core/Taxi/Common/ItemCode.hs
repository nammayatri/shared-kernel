module Beckn.Types.Core.Taxi.Common.ItemCode
  ( module Beckn.Types.Core.Taxi.Common.ItemCode,
    module Reexport,
  )
where

import Beckn.Prelude hiding (show)
import Beckn.Types.Common
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.Common.VehicleVariant as Reexport
import Data.Aeson
import Data.OpenApi
import qualified Data.Text as T
import GHC.Show (show)

data ItemCode = ItemCode
  { fareProductType :: FareProductType,
    vehicleVariant :: VehicleVariant,
    distance :: Maybe Kilometers, -- only RentalTrip
    duration :: Maybe Hours -- only RentalTrip
  }
  deriving (Generic, Eq)

instance Show ItemCode where
  show (ItemCode ONE_WAY_TRIP vehVar _ _) =
    show ONE_WAY_TRIP <> "_" <> show vehVar
  show (ItemCode RENTAL_TRIP vehVar dist dur) =
    show RENTAL_TRIP <> "_" <> show vehVar <> "_" <> show dist <> "_" <> show dur

instance Read ItemCode where
  readsPrec d r' = do
    readParen
      (d > app_prec)
      ( \r ->
          [ (ItemCode ONE_WAY_TRIP v1 Nothing Nothing, r2)
            | ("ONE_WAY_TRIP_", r1) <- lex r,
              (v1, r2) <- readsPrec d r1
          ]
      )
      r'
      ++ readParen
        (d > app_prec)
        ( \r ->
            [ (ItemCode ONE_WAY_TRIP v1 (Just v2) (Just v3), r6)
              | ("RENTAL_TRIP_", r1) <- lex r,
                (v1, r2) <- readsPrec d r1,
                ("_", r3) <- lex r2,
                (v2, r4) <- readsPrec d r3,
                ("_", r5) <- lex r4,
                (v3, r6) <- readsPrec d r5
            ]
        )
        r'
    where
      app_prec = 10

instance ToJSON ItemCode where
  toJSON = String . T.pack . show

instance FromJSON ItemCode where
  parseJSON = withText "FareProductType" $ \s -> do
    return . read $ T.unpack s

instance ToSchema ItemCode where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
