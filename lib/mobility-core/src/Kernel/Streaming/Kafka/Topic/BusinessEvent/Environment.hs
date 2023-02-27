{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment where

import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.App (getPodName)

type HasKafkaBE r kafkaEnvs = (HasField "kafkaEnvs" r kafkaEnvs, HasField "businessEventEnv" kafkaEnvs KafkaBEEnv)

data KafkaBEEnv = KafkaBEEnv
  { hostName :: KafkaHostName,
    serviceName :: KafkaServiceName
  }
  deriving (Generic)

buildKafkaBEEnv :: KafkaServiceName -> IO KafkaBEEnv
buildKafkaBEEnv serviceName = do
  hostName <- getPodName
  return $
    KafkaBEEnv
      { ..
      }
