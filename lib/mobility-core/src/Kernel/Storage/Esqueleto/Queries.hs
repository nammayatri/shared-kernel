{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Kernel.Storage.Esqueleto.Queries
  ( findOne,
    findOne',
    findById,
    findById',
    findAll,
    findAll',
    create,
    create',
    update,
    update',
    createMany,
    createMany',
    createUnique,
    createUnique',
    updateReturningCount,
    updateReturningCount',
    deleteByKey,
    deleteByKey',
    delete,
    delete',
    deleteReturningCount,
    deleteReturningCount',
    repsert,
    repsert',
    upsert,
    upsert',
    upsertBy,
    upsertBy',
    insertSelect,
    insertSelect',
    insertSelectCount,
    insertSelectCount',
    module EsqExport,
    module Reexport,
  )
where

import Data.Text (pack)
import Data.Typeable
import Database.Esqueleto.Experimental as EsqExport hiding
  ( Table,
    delete,
    deleteCount,
    deleteKey,
    insert,
    insertSelect,
    insertSelectCount,
    rand,
    random_,
    rawSql,
    repsert,
    selectOne,
    update,
    updateCount,
    upsert,
    upsertBy,
    (<&>),
  )
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Esqueleto.Internal.Internal as Esq
import Database.Persist.Postgresql hiding (delete, rawSql, repsert, update, upsert, upsertBy)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Class
import Kernel.Storage.Esqueleto.DTypeBuilder
import Kernel.Storage.Esqueleto.Queries.Raw as Reexport
import Kernel.Storage.Esqueleto.SqlDB
import Kernel.Storage.Esqueleto.Transactionable
import Kernel.Types.Logging (Log)

findOne :: (Typeable t, Transactionable m, Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> m (Maybe a)
findOne = buildDType . findOneInternal

findOne' :: (Typeable t, Transactionable m, TEntity t a, Esq.SqlSelect b t) => Esq.SqlQuery b -> DTypeBuilder m (Maybe a)
findOne' q = extractTType <$> findOneInternal q

findOneInternal :: forall m t b. (Typeable t, Transactionable m, Esq.SqlSelect b t) => Esq.SqlQuery b -> DTypeBuilder m (Maybe t)
findOneInternal q = liftToBuilder . runTransaction . SelectSqlDB . SqlDB $ selectOnlyOne
  where
    selectOnlyOne = do
      list <- lift $ Esq.select q
      case list of
        [res] -> return $ Just res
        [] -> return Nothing
        _ -> do
          let errType = pack . show . typeRep $ (Proxy @t)
          throw $ PersistError $ "Multiple results of " <> errType

findById :: forall a t m. (Typeable t, Transactionable m, QEntity (Entity t) a, TEntityKey t) => DomainKey t -> m (Maybe a)
findById = buildDType . findByIdInternal @t

findById' :: forall t m. (Typeable t, Transactionable m, TEntityKey t, TEntity (Entity t) t) => DomainKey t -> DTypeBuilder m (Maybe t)
findById' dkey = extractTType <$> findByIdInternal @t dkey

findByIdInternal :: forall t m. (Typeable t, Transactionable m, TEntityKey t, Log m) => DomainKey t -> DTypeBuilder m (Maybe (Entity t))
findByIdInternal dkey = findOneInternal $ do
  let key = toKey @t dkey
  res <- from $ table @t
  where_ $ res Esq.^. persistIdField Esq.==. val key
  return res

findAll :: (Transactionable m, Esq.SqlSelect b t, QEntity [t] [a]) => Esq.SqlQuery b -> m [a]
findAll q = buildDType $ findAllInternal q

findAll' :: (Transactionable m, Esq.SqlSelect b t, TEntity [t] [a]) => Esq.SqlQuery b -> DTypeBuilder m [a]
findAll' q = extractTType <$> findAllInternal q

findAllInternal :: (Transactionable m, Esq.SqlSelect b t) => Esq.SqlQuery b -> DTypeBuilder m [t]
findAllInternal q = liftToBuilder . runTransaction . SelectSqlDB . SqlDB $ lift (Esq.select q)

create ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    ToTType t a
  ) =>
  a ->
  SqlDB ()
create q = do
  let ttypes = toTType q
  SqlDB . lift $ Esq.insert_ ttypes

create' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  t ->
  FullEntitySqlDB ()
create' q = do
  liftToFullEntitySqlDB . SqlDB . lift $ Esq.insert_ q

createMany ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    ToTType t a
  ) =>
  [a] ->
  SqlDB ()
createMany q = do
  let ttypes = toTType `fmap` q
  SqlDB . lift $ Esq.insertMany_ ttypes

createMany' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  [t] ->
  FullEntitySqlDB ()
createMany' q = do
  liftToFullEntitySqlDB . SqlDB . lift $ Esq.insertMany_ q

createUnique ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    ToTType t a
  ) =>
  a ->
  SqlDB (Maybe (Key t))
createUnique q = do
  let ttypes = toTType q
  SqlDB . lift $ Esq.insertUnique ttypes

createUnique' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  t ->
  FullEntitySqlDB (Maybe (Key t))
createUnique' q = do
  liftToFullEntitySqlDB . SqlDB . lift $ Esq.insertUnique q

update ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  SqlDB ()
update = SqlDB . lift . Esq.update

update' ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  FullEntitySqlDB ()
update' = liftToFullEntitySqlDB . SqlDB . lift . Esq.update

updateReturningCount ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  SqlDB Int64
updateReturningCount = SqlDB . lift . Esq.updateCount

updateReturningCount' ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  FullEntitySqlDB Int64
updateReturningCount' = liftToFullEntitySqlDB . SqlDB . lift . Esq.updateCount

deleteByKey ::
  forall t.
  ( TEntityKey t
  ) =>
  DomainKey t ->
  SqlDB ()
deleteByKey = SqlDB . lift . Esq.deleteKey . toKey @t

deleteByKey' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  Key t ->
  FullEntitySqlDB ()
deleteByKey' = liftToFullEntitySqlDB . SqlDB . lift . Esq.deleteKey

delete ::
  Esq.SqlQuery () ->
  SqlDB ()
delete = SqlDB . lift . Esq.delete

delete' ::
  Esq.SqlQuery () ->
  FullEntitySqlDB ()
delete' = liftToFullEntitySqlDB . SqlDB . lift . Esq.delete

deleteReturningCount ::
  Esq.SqlQuery () ->
  SqlDB Int64
deleteReturningCount = SqlDB . lift . Esq.deleteCount

deleteReturningCount' ::
  Esq.SqlQuery () ->
  FullEntitySqlDB Int64
deleteReturningCount' = liftToFullEntitySqlDB . SqlDB . lift . Esq.deleteCount

repsert ::
  ( PersistEntityBackend t ~ SqlBackend,
    ToTType t a,
    TEntityKey t
  ) =>
  DomainKey t ->
  a ->
  SqlDB ()
repsert k v = do
  let ttype = toTType v
  SqlDB . lift $ Esq.repsert (toKey k) ttype

repsert' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  Key t ->
  t ->
  FullEntitySqlDB ()
repsert' k v = do
  liftToFullEntitySqlDB . SqlDB . lift $ Esq.repsert k v

upsert ::
  ( OnlyOneUniqueKey t,
    PersistEntityBackend t ~ SqlBackend,
    ToTType t a
  ) =>
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  SqlDB ()
upsert r u = do
  let uniqueKey = onlyUniqueP $ toTType r
  upsertBy uniqueKey r u

upsert' ::
  ( OnlyOneUniqueKey t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  t ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  FullEntitySqlDB ()
upsert' r u = do
  let uniqueKey = onlyUniqueP r
  upsertBy' uniqueKey r u

upsertBy ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend,
    ToTType t a
  ) =>
  Unique t ->
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  SqlDB ()
upsertBy k r u = do
  mbEntity <- SqlDB . lift $ getBy k
  case mbEntity of
    Nothing -> create r
    Just ent -> update $ \tbl -> do
      Esq.set
        tbl
        u
      where_ $ tbl Esq.^. persistIdField Esq.==. val (entityKey ent)

upsertBy' ::
  ( PersistEntity t,
    PersistEntityBackend t ~ SqlBackend
  ) =>
  Unique t ->
  t ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  FullEntitySqlDB ()
upsertBy' k r u = do
  mbEntity <- liftToFullEntitySqlDB . SqlDB . lift $ getBy k
  case mbEntity of
    Nothing -> create' r
    Just ent -> update' $ \tbl -> do
      Esq.set
        tbl
        u
      where_ $ tbl Esq.^. persistIdField Esq.==. val (entityKey ent)

insertSelect ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  SqlDB ()
insertSelect = SqlDB . lift . Esq.insertSelect

insertSelect' ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  FullEntitySqlDB ()
insertSelect' = liftToFullEntitySqlDB . SqlDB . lift . Esq.insertSelect

insertSelectCount ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  SqlDB Int64
insertSelectCount = SqlDB . lift . Esq.insertSelectCount

insertSelectCount' ::
  ( PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  FullEntitySqlDB Int64
insertSelectCount' = liftToFullEntitySqlDB . SqlDB . lift . Esq.insertSelectCount
