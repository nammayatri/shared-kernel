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
import Data.Text as T hiding (elem, map)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified EulerHS.KVConnector.Types as KV
import EulerHS.KVConnector.Utils (getShardedHashTag)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Kafka.Producer as KafkaProd
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Types.Error
import Kernel.Utils.Common
import Text.Casing (camel, quietSnake)

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
  KV.TableMappings (table Identity) =>
  [table Identity] ->
  HashMap Text Text
getMappings _ = HM.fromList (map (bimap T.pack T.pack) (KV.getTableMappings @(table Identity)))

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
  if modelName == "atlas_driver_offer_bpp" then pure ("adob-sessionizer-" <> topicName) else pure ("aap-sessionizer-" <> topicName)

getKeyForKafka :: Text -> Text
getKeyForKafka pKeyText = do
  let shard = getShardedHashTag pKeyText
  pKeyText <> shard

tableInKafka :: L.MonadFlow m => Text -> m Bool
tableInKafka modelName = do
  tables <- L.getOption KBT.Tables
  case tables of
    Nothing -> pure False
    Just tables' -> pure $ modelName `elem` (tables'.kafkaNonKVTables)
