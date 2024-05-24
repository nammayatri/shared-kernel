{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Beam.Lib.Utils where

import Data.Aeson as A
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as Serialize
import Data.Text as T hiding (elem, map)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Beam.Postgres
import EulerHS.KVConnector.Types
import qualified EulerHS.KVConnector.Utils as EKU
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Kafka.Producer as KafkaProd
import Kernel.Beam.Types (KafkaConn (..))
import Kernel.Types.App
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (throwError)
import Sequelize
import Text.Casing (camel, quietSnake)

-------------------------------------- classes --------------------------------------

-- classes for converting from beam types to ttypes and vice versa
class
  FromTType' t a
    | t -> a
  where
  fromTType' :: KvDbFlow m r => t -> m (Maybe a)

class
  ToTType' t a
    | a -> t
  where
  toTType' :: a -> t

-- Below class FromTType'' and ToTType'' are only to be used with scheduler
class
  FromTType'' t a
    | a -> t
  where
  fromTType'' :: (MonadThrow m, Log m, KvDbFlow m r) => t -> m (Maybe a)

class
  ToTType'' t a
    | a -> t
  where
  toTType'' :: a -> t

-- Below class FromCacType'' are only to be used with cac.
class
  FromCacType t a
    | t -> a
  where
  fromCacType :: KvDbFlow m r => t -> m (Maybe a)

-------------------------------------- data types --------------------------------------

meshConfig :: MeshConfig
meshConfig =
  MeshConfig
    { meshEnabled = False,
      memcacheEnabled = False,
      meshDBName = "postgres",
      ecRedisDBStream = "driver-db-sync-stream",
      kvRedis = "KVRedis",
      redisTtl = 18000,
      kvHardKilled = True,
      cerealEnabled = False
    }

data DbFunctions = DbFunctions
  { createInternalFunction :: forall table m r a. (BeamTableFlow table m r) => MeshConfig -> (a -> table Identity) -> a -> m (),
    findOneInternalFunction :: forall table m r a. (BeamTableFlow table m r) => MeshConfig -> (table Identity -> m (Maybe a)) -> Where Postgres table -> m (Maybe a),
    findAllInternalFunction :: forall table m r a. (BeamTableFlow table m r) => MeshConfig -> (table Identity -> m (Maybe a)) -> Where Postgres table -> m [a],
    findAllWithOptionsInternalFunction :: forall table m r a. (BeamTableFlow table m r) => MeshConfig -> (table Identity -> m (Maybe a)) -> Where Postgres table -> OrderBy table -> Maybe Int -> Maybe Int -> m [a],
    findAllWithOptionsInternalFunction' :: forall table m r a. (BeamTableFlow table m r) => MeshConfig -> (table Identity -> m (Maybe a)) -> Where Postgres table -> Maybe Int -> Maybe Int -> m [a],
    findAllWithKVAndConditionalDBInternalFunction :: forall table m r a. (BeamTableFlow table m r) => MeshConfig -> (table Identity -> m (Maybe a)) -> Where Postgres table -> Maybe (OrderBy table) -> m [a],
    updateInternalFunction :: forall table m r. (BeamTableFlow table m r) => MeshConfig -> [Set Postgres table] -> Where Postgres table -> m (),
    updateOneInternalFunction :: forall table m r. (BeamTableFlow table m r) => MeshConfig -> [Set Postgres table] -> Where Postgres table -> m (),
    deleteInternalFunction :: forall table m r. (BeamTableFlow table m r) => MeshConfig -> Where Postgres table -> m ()
  }

-------------------------------------- types --------------------------------------

type KvDbFlow m r = (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "dbFunctions" r DbFunctions)

type BeamTableFlow table m r =
  ( HasCallStack,
    BeamTable table,
    MonadFlow m,
    EsqDBFlow m r
  )

type BeamTable table =
  ( Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    TableMappings (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity)
  )

-------------------------------------- functions --------------------------------------

textToSnakeCaseText :: Text -> Text
textToSnakeCaseText = T.pack . quietSnake . T.unpack

replaceMappings :: A.Value -> HM.HashMap Text Text -> A.Value
replaceMappings (A.Object obj) mapp =
  A.Object $
    AKM.fromList $
      map (\(key, val) -> (AesonKey.fromText (textToSnakeCaseText $ fromMaybe (AesonKey.toText key) (HM.lookup (AesonKey.toText key) mapp)), convertIntoValidValForCkh val)) $
        AKM.toList obj
replaceMappings x _ = x

convertIntoValidValForCkh :: A.Value -> A.Value
convertIntoValidValForCkh dbValue = case dbValue of
  A.String text' -> case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack text')
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" (T.unpack text') of
    Nothing -> A.String text'
    Just x -> A.Number $ fromInteger $ round (realToFrac (utcTimeToPOSIXSeconds x) :: Double)
  A.Bool val -> if val then A.String "True" else A.String "False"
  _ -> dbValue

getMappings ::
  forall table.
  TableMappings (table Identity) =>
  [table Identity] ->
  HashMap Text Text
getMappings _ = HM.fromList (map (bimap T.pack T.pack) (getTableMappings @(table Identity)))

pushToKafka :: (MonadFlow m, ToJSON a) => a -> Text -> Text -> m ()
pushToKafka messageRecord topic key = do
  kafkaProducerTools <- L.getOption KafkaConn
  case kafkaProducerTools of
    Nothing -> throwError $ InternalError "Kafka producer tools not found"
    Just kafkaProducerTools' -> do
      mbErr <- KafkaProd.produceMessage kafkaProducerTools'.producer (kafkaMessage topic messageRecord key)
      whenJust mbErr (throwError . KafkaUnableToProduceMessage)

kafkaMessage :: ToJSON a => Text -> a -> Text -> KafkaProd.ProducerRecord
kafkaMessage topicName event key =
  KafkaProd.ProducerRecord
    { prTopic = KafkaProd.TopicName topicName,
      prPartition = KafkaProd.UnassignedPartition,
      prKey = Just $ TE.encodeUtf8 key,
      prValue = Just . LBS.toStrict $ encode event
    }

getKafkaTopic :: (MonadFlow m) => Maybe Text -> Text -> m Text
getKafkaTopic mSchema model = do
  let topicName = T.toLower $ T.pack (camel (T.unpack model))
  modelName <- maybe (L.throwException $ InternalError "Schema not found") pure mSchema
  case modelName of
    "atlas_driver_offer_bpp" -> pure ("adob-sessionizer-" <> topicName)
    "atlas_app" -> pure ("aap-sessionizer-" <> topicName)
    "atlas_bpp_dashboard" -> pure ("abd-sessionizer-" <> topicName)
    "atlas_bap_dashboard" -> pure ("aad-sessionizer-" <> topicName)
    r -> pure (r <> "-sessionizer-" <> topicName)

getKeyForKafka :: Text -> Text
getKeyForKafka pKeyText = do
  let shard = EKU.getShardedHashTag pKeyText
  pKeyText <> shard

-- commenting in case we need it in future
-- tableInKafka :: L.MonadFlow m => Text -> m Bool
-- tableInKafka modelName = do
--   tables <- L.getOption KBT.Tables
--   case tables of
--     Nothing -> pure False
--     Just tables' -> pure $ modelName `elem` (tables'.kafkaNonKVTables)
