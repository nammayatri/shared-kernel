{-# LANGUAGE AllowAmbiguousTypes #-}

module Kernel.Storage.Esqueleto.DeletedEntity
  ( module Kernel.Storage.Esqueleto.DeletedEntity,
    module Reexport,
  )
where

import qualified Database.Esqueleto.Internal.Internal as Esq (SqlSelect)
import Database.Persist.Postgresql as Esq (tableName)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.DTypeBuilder as Esq
import qualified Kernel.Storage.Esqueleto.Queries as Esq
import Kernel.Storage.Tabular.DeletedEntity (DeletedEntityT)
import Kernel.Types.DeletedEntity as Reexport
import Kernel.Types.Id
import Kernel.Utils.Common

buildDeletedEntity :: forall t m. (MonadGuid m, MonadTime m, Esq.PersistEntity t, ToJSON t) => DeletedBy -> t -> m DeletedEntity
buildDeletedEntity deletedBy entity = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let primaryId = Esq.keyFromRecordM <&> (\keyFromRecord -> encodeToText $ keyFromRecord entity)
  pure
    DeletedEntity
      { id,
        primaryId,
        tableName = Esq.tableName entity,
        rowData = encodeToText entity,
        deletedBy = deletedBy,
        deletedOn = now
      }

-- | Deleted entity will be automatically preserved into `deleted_entity` table
deleteByIdP ::
  forall t.
  ( Typeable t,
    Esq.TEntityKey t,
    ToJSON t
  ) =>
  DeletedBy ->
  Esq.DomainKey t ->
  Esq.SqlDB ()
deleteByIdP deletedBy entityId = Esq.doNotBuildDType $ do
  mbEntityT <- Esq.findById' @t entityId
  Esq.liftToBuilder $ do
    whenJust mbEntityT $ \entityT -> do
      deletedEntity <- buildDeletedEntity deletedBy entityT
      Esq.deleteByKey @t entityId
      Esq.create @DeletedEntityT deletedEntity

-- | All deleted entities will be automatically preserved into `deleted_entity` table
deleteP ::
  forall t b.
  ( Esq.SqlSelect b (Esq.Entity t),
    Esq.PersistEntity t,
    ToJSON t
  ) =>
  DeletedBy ->
  Esq.SqlQuery b ->
  Esq.SqlDB ()
deleteP deletedBy query = Esq.doNotBuildDType $ do
  entityTs <- Esq.findAllInternal query
  Esq.liftToBuilder $ do
    deletedEntities <- forM entityTs $ \entityT ->
      buildDeletedEntity deletedBy $ Esq.entityVal entityT
    Esq.delete $ void query
    Esq.createMany @DeletedEntityT deletedEntities
