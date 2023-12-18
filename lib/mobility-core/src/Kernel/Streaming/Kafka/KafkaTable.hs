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
import Kernel.Prelude

-- KafkaTable type should not be changed because kafka consumer could not handle old messages
data KafkaTable = KafkaTable
  { schemaName :: Text,
    tableName :: Text,
    tableContent :: A.Value,
    timestamp :: UTCTime
  }
  deriving (ToJSON, FromJSON, Generic)

-- FIXME use 24 partitions instead of 24 topics
countTopicNumber :: UTCTime -> Int
countTopicNumber timeStamp = do
  let timeOfDay = Time.timeToTimeOfDay . Time.utctDayTime $ timeStamp
  Time.todHour timeOfDay
