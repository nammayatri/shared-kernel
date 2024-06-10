module Kernel.Types.Cac where

import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import Data.Maybe
import qualified Data.Text as Text
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, logDebug, logError)
import Kernel.Utils.Error.Throwing (throwError)
import System.Random

newtype CacKeyValue = CacKeyValue
  { getCacKeyValue :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

fromJSONHelper :: (MonadFlow m, FromJSON a) => Value -> Text -> m (Maybe a)
fromJSONHelper k tn = case fromJSON k of
  Success a -> pure (Just a)
  DA.Error err -> do
    logError $ "Error in parsing the value for the table: " <> tn <> " with error: " <> Text.pack err
    pure Nothing

dropPrefixFromConfig :: Text.Text -> Key -> Key
dropPrefixFromConfig key config = maybe config DAK.fromText $ Text.stripPrefix key (DAK.toText config)

initializeCACThroughConfig :: (CacheFlow m r, EsqDBFlow m r) => (String -> Int -> String -> String -> IO Int) -> Text -> String -> String -> Int -> m ()
initializeCACThroughConfig func config tenant' host interval = do
  status <- liftIO $ func tenant' interval (Text.unpack config) host
  logDebug $ "status of creating the client for tenant backup " <> Text.pack tenant' <> " is " <> Text.pack (show status)
  if status == 0
    then pure ()
    else do
      incrementSystemConfigsFailedCounter "cac_client_creation_failure"
      throwError $ InternalError $ "error in creating the client for tenant" <> Text.pack tenant'

data CACData = CACData
  { id :: Text,
    idType :: Text,
    context :: Text,
    configName :: Text,
    variantIds :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

getToss :: (CacheFlow m r) => Maybe Text -> m Int
getToss srId = do
  gen <- newStdGen
  expTime <- fromIntegral <$> asks (.cacConfig.cacExpTime)
  let (toss', _) = randomR (1, 100) gen :: (Int, StdGen)
  maybe
    (pure toss')
    ( \srId' -> do
        Hedis.withCrossAppRedis (Hedis.safeGet (makeCACConfigKey srId')) >>= \case
          Just (a :: Int) -> pure a
          Nothing -> do
            _ <- Hedis.withCrossAppRedis $ Hedis.setExp (makeCACConfigKey srId') toss' expTime
            pure toss'
    )
    srId

makeCACConfigKey :: Text -> Text
makeCACConfigKey id = "CAC:STKID" <> id

getConfigFromMemoryCommon :: (CacheFlow m r, EsqDBFlow m r, T.OptionEntity b a) => b -> Bool -> (String -> IO Bool) -> m (Maybe a)
getConfigFromMemoryCommon configCondition isExpired expmt = do
  config <- L.getOption configCondition
  tenant' <- asks (.cacConfig.tenant)
  isExperiment <- liftIO $ expmt tenant'
  if isExpired || isExperiment
    then pure Nothing
    else pure config
