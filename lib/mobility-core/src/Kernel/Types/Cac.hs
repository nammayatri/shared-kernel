module Kernel.Types.Cac where

import Data.Aeson
import qualified Data.Aeson.Key as DAK
import Data.Maybe
import qualified Data.Text as Text
import Kernel.Prelude
import Kernel.Utils.Common
import qualified System.Environment as Se

data CACValue a = CACValue
  { cacValue :: Value
  }

class FromJSONCAC a where
  fromJSONCAC :: FromJSON a => CACValue a -> Result a

dropPrefixFromConfig :: Text.Text -> Key -> Key
dropPrefixFromConfig key config =
  case Text.stripPrefix key (DAK.toText config) of
    Just a -> DAK.fromText a
    Nothing -> config

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => (String -> Int -> String -> String -> IO Int) -> Text -> m ()
initializeCACThroughConfig func config = do
  host <- liftIO $ Se.lookupEnv "CAC_HOST"
  interval' <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  interval <- pure $ fromMaybe 10 (readMaybe =<< interval')
  tenant <- liftIO (Se.lookupEnv "DRIVER_TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  status <- liftIO $ func tenant interval (Text.unpack config) (fromMaybe "http://localhost:8080" host)
  case status of
    0 -> pure ()
    _ -> error $ "error in creating the client for tenant" <> Text.pack tenant
