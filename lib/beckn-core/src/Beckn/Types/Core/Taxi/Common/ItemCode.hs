module Beckn.Types.Core.Taxi.Common.ItemCode
  ( module Beckn.Types.Core.Taxi.Common.ItemCode,
    module Reexport,
  )
where

import Beckn.Prelude hiding (show)
import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.Common.VehicleVariant as Reexport
import Data.Aeson
import Data.OpenApi
import Data.Text
import GHC.Show (show)

data ItemCode = ItemCode
  { fareProductType :: FareProductType,
    vehicleVariant :: VehicleVariant
  }
  deriving (Eq, Generic)

instance Show ItemCode where
  show (ItemCode a b) = show a <> "/" <> show b

instance Read ItemCode where
  readsPrec d r' = do
    readParen
      (d > app_prec)
      ( \r ->
          [ (ItemCode v1 v2, r3)
            | (v1, r1) <- readsPrec d r,
              ("/", r2) <- lex r1,
              (v2, r3) <- readsPrec d r2
          ]
      )
      r'
    where
      app_prec = 10

instance ToJSON ItemCode where
  toJSON = String . pack . show

instance FromJSON ItemCode where
  parseJSON = withText "ItemCode" $ \s -> do
    return . read $ unpack s

instance ToSchema ItemCode where
  declareNamedSchema _ = do
    declareNamedSchema (Proxy :: Proxy Text)
