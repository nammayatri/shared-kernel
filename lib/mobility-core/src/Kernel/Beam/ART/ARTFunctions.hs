{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Kernel.Beam.ART.ARTFunctions where

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
import Kernel.Beam.Lib.Utils
import Kernel.Beam.Types (KafkaConn (..))
import Kernel.Types.App
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Error.Throwing (throwError)
import Sequelize
import Text.Casing (camel, quietSnake)

findOneInternalArt ::
  forall table m r a.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m (Maybe a)
findOneInternalArt = error "not implemented"

findAllInternalArt ::
  forall table m r a.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  m [a]
findAllInternalArt = error "not implemented"

findAllWithOptionsInternalArt ::
  forall table m r a.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  (table Identity -> m (Maybe a)) ->
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsInternalArt = error "not implemented"

updateInternalArt ::
  forall table m r.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateInternalArt = error "not implemented"

updateOneInternalArt ::
  forall table m r.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateOneInternalArt = error "not implemented"

createInternalArt ::
  forall table m r a.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  (a -> table Identity) ->
  a ->
  m ()
createInternalArt = error "not implemented"

deleteInternalArt ::
  forall table m r.
  (BeamTableFlow table m, EsqDBFlow m r) =>
  MeshConfig ->
  Where Postgres table ->
  m ()
deleteInternalArt = error "not implemented"
