module Beckn.Types.Core.Taxi.Common.FareProductType where

import Beckn.Prelude hiding (show)
import Data.Aeson
import qualified Data.Text as T
import GHC.Show (show)

data FareProductType
  = ONE_WAY_TRIP
  | RENTAL_TRIP
      { baseDistanceKm :: Int,
        baseDuration :: Int
      }
  deriving
    ( Eq,
      Ord,
      Generic,
      ToSchema
      -- ToParamSchema
    )

instance Show FareProductType where
  show ONE_WAY_TRIP = "ONE_WAY_TRIP"
  show (RENTAL_TRIP dist hr) = "RENTAL_TRIP_" <> show dist <> "_" <> show hr

instance Read FareProductType where
  readsPrec d r' = do
    readParen
      (d > app_prec)
      ( \r ->
          [ (ONE_WAY_TRIP, r1)
            | ("ONE_WAY_TRIP", r1) <- lex r
          ]
      )
      r'
      ++ readParen
        (d > app_prec)
        ( \r ->
            [ (RENTAL_TRIP v1 v2, r4)
              | ("RENTAL_TRIP_", r1) <- lex r,
                (v1, r2) <- readsPrec d r1,
                ("_", r3) <- lex r2,
                (v2, r4) <- readsPrec d r3
            ]
        )
        r'
    where
      app_prec = 10

instance ToJSON FareProductType where
  toJSON = String . T.pack . show

instance FromJSON FareProductType where
  parseJSON = withText "FareProductType" $ \s -> do
    return . read $ T.unpack s
