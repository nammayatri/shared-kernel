-- support only read operation for now
module Kernel.Storage.Clickhouse.Queries
  ( findAll,
    findOne,
    runRawQuery',
    findOneWithOrder,
  )
where

import Data.Text as T
import Data.Typeable (typeRep)
import Database.ClickHouseDriver.HTTP
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Clickhouse.Types
import Kernel.Types.Error
import Kernel.Utils.Common hiding (Limit, Offset)
import Kernel.Utils.JSON

appendExpr :: ClickhouseExpr -> String -> String
appendExpr expr prefix =
  prefix
    <> if expr /= Nil
      then " WHERE " <> toClickhouseQuery expr
      else ""

__select :: forall a. Typeable a => Proxy a -> ClickhouseExpr -> String
__select proxyTableType expr = do
  let tableName = dropBeforeDot $ camelToSnakeCase . show $ typeRep proxyTableType
  appendExpr expr ("SELECT * FROM " <> tableName)

addClause :: ToClickhouseQuery a => String -> a -> String
addClause query clause = query <> toClickhouseQuery clause

__limit :: String -> Limit -> String
__limit = addClause

__orderBy :: String -> Order -> String
__orderBy = addClause

__offset :: String -> Offset -> String
__offset = addClause

runClickhouse :: (MonadFlow m, ClickhouseFlow m env, FromJSON a) => (HttpConnection -> IO (Either String a)) -> m (Either String a)
runClickhouse action = do
  con <- asks (.clickhouseEnv.connection)
  L.runIO $ action con

runRawQuery :: (MonadFlow m, ClickhouseFlow m env, FromJSON a) => String -> m (Either String a)
runRawQuery query = runClickhouse (`runQuery` getJSON query)

runRawQuery' :: (MonadFlow m, ClickhouseFlow m env, FromJSON a) => ClickhouseExpr -> m (Either String a)
runRawQuery' (ExprStr query) = runRawQuery query
runRawQuery' _ = throwError $ InternalError "can't call this function with unresolved clickhouseExpr"

constructQuery :: Typeable a => Proxy a -> ClickhouseExpr -> Maybe Limit -> Maybe Offset -> Maybe Order -> String
constructQuery proxyTable expr mbLimit mbOffset mbOrder = do
  addOffset . addLimit . addOrder $ __select proxyTable expr
  where
    addLimit q = maybe q (__limit q) mbLimit
    addOrder q = maybe q (__orderBy q) mbOrder
    addOffset q = maybe q (__offset q) mbOffset

findAll :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => Proxy a -> ClickhouseExpr -> Maybe Limit -> Maybe Offset -> Maybe Order -> m (Either String [a])
findAll proxyTable expr mbLimit mbOffset mbOrder = runRawQuery $ constructQuery proxyTable expr mbLimit mbOffset mbOrder

findOne' :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => Proxy a -> ClickhouseExpr -> Maybe Offset -> Maybe Order -> m (Maybe a)
findOne' proxyTable expr mbOffset mbOrder = either (\err -> logInfo (T.pack err) $> Nothing) (pure . listToMaybe) =<< runRawQuery (constructQuery proxyTable expr (Just $ Limit 1) mbOffset mbOrder)

findOne :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => Proxy a -> ClickhouseExpr -> m (Maybe a)
findOne proxyTable expr = findOne' proxyTable expr Nothing Nothing

findOneWithOrder :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => Proxy a -> ClickhouseExpr -> Order -> m (Maybe a)
findOneWithOrder proxyTable expr order = findOne' proxyTable expr Nothing (Just order)

-- USAGE EXAPLE
-- res1 :: Either String [Test] <- CH.runRawQuery' $ CH.ExprStr "SELECT * FROM test"
-- res2 :: (Maybe Test) <- CH.findOne (Proxy @Test) ("x" =.= "1")
-- res3 :: (Maybe Test) <- CH.findOneWithOrder (Proxy @Test) ("x" =.= "1") (CH.Desc "x")
-- res4 :: Either String [Test] <- CH.findAll (Proxy @Test) ("x" =.= "1") (Just $ CH.Limit 10) Nothing (Just $ CH.Desc "x")
