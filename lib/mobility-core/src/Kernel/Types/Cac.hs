module Kernel.Types.Cac where

import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import Data.Char
import Data.Maybe
import qualified Data.Text as Text
import Kernel.Prelude
import Kernel.Utils.Common

data CACValue a = CACValue
  { cacValue :: Value
  }

class FromJSONCAC a where
  fromJSONCAC :: FromJSON a => CACValue a -> Result a

fromJSONHelper :: FromJSON a => Value -> Maybe a
fromJSONHelper k = case fromJSON k of
  Success a -> Just a
  DA.Error _ -> Nothing

dropPrefixFromConfig :: Text.Text -> Key -> Key
dropPrefixFromConfig key config =
  case Text.stripPrefix key (DAK.toText config) of
    Just a -> DAK.fromText a
    Nothing -> config

cacTableCaseInsensitive :: String -> String
cacTableCaseInsensitive [] = []
cacTableCaseInsensitive ('_' : xs) = cacTableCaseInsensitive xs
cacTableCaseInsensitive ('-' : xs) = cacTableCaseInsensitive xs
cacTableCaseInsensitive (x : xs) = toLower x : cacTableCaseInsensitive xs

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => (String -> Int -> String -> String -> IO Int) -> Text -> String -> String -> Int -> m ()
initializeCACThroughConfig func config tenant host interval = do
  status <- liftIO $ func tenant interval (Text.unpack config) host
  logDebug $ "status of creating the client for tenant backup " <> Text.pack tenant <> " is " <> Text.pack (show status)
  case status of
    0 -> pure ()
    _ -> error $ "error in creating the client for tenant" <> Text.pack tenant
