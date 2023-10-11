{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.KafkaTable where

import qualified Data.Aeson as A
import qualified Data.Time as Time
import qualified Kernel.Beam.Lib.UtilsTH as TH
import Kernel.Prelude
import Sequelize

-- KafkaTable type should not be changed because kafka consumer could not handle old messages
data KafkaTable = KafkaTable
  { schemaName :: Text,
    tableName :: Text,
    tableContent :: A.Value,
    timestamp :: UTCTime
  }
  deriving (ToJSON, FromJSON, Generic)

mkKafkaTable ::
  forall table.
  ( Sequelize.ModelMeta table,
    TH.HasSchemaName table,
    ToJSON (table Identity)
  ) =>
  table Identity ->
  UTCTime ->
  KafkaTable
mkKafkaTable table timestamp = do
  let schemaName = TH.schemaName (Proxy @table)
  let tableName = modelTableName @table
  let tableContent = toJSON table
  KafkaTable {..}

-- FIXME use 24 partitions instead of 24 topics
countTopicNumber :: UTCTime -> Int
countTopicNumber timeStamp = do
  let timeOfDay = Time.timeToTimeOfDay . Time.utctDayTime $ timeStamp
  Time.todHour timeOfDay
