module Kernel.Types.Cac where

import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import Data.Maybe
import qualified Data.Text as Text
import Kernel.Prelude
import Kernel.Utils.Common

fromJSONHelper :: FromJSON a => Value -> Maybe a
fromJSONHelper k = case fromJSON k of
  Success a -> Just a
  DA.Error _ -> Nothing

dropPrefixFromConfig :: Text.Text -> Key -> Key
dropPrefixFromConfig key config = maybe config DAK.fromText $ Text.stripPrefix key (DAK.toText config)

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => (String -> Int -> String -> String -> IO Int) -> Text -> String -> String -> Int -> m ()
initializeCACThroughConfig func config tenant host interval = do
  status <- liftIO $ func tenant interval (Text.unpack config) host
  logDebug $ "status of creating the client for tenant backup " <> Text.pack tenant <> " is " <> Text.pack (show status)
  case status of
    0 -> pure ()
    _ -> error $ "error in creating the client for tenant" <> Text.pack tenant

data CACData = CACData
  { id :: Text,
    idType :: Text,
    context :: Text,
    configName :: Text,
    variantIds :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
