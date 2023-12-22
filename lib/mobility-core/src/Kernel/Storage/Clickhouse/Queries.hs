-- support only read operation for now
module Kernel.Storage.Clickhouse.Queries
  ( findAll,
    findOne,
    runRawQuery',
    findOneWithOrder,
  )
where

import Data.Text as T hiding (null)
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

appendGroupBy :: Maybe String -> String -> String
appendGroupBy maybeGroupBy query = do
  let groupBy_ = case maybeGroupBy of
        Just gb -> " GROUP BY " <> gb
        Nothing -> ""
  query <> groupBy_

__select :: forall a. Typeable a => Proxy a -> [Text] -> Maybe String -> ClickhouseExpr -> String
__select proxyTableType cols maybeGroupBy expr = do
  let tableName = dropBeforeDot $ camelToSnakeCase . show $ typeRep proxyTableType
  let selectClause = if null cols then "SELECT *" else T.unpack $ "SELECT " <> intercalate ", " cols
  appendGroupBy maybeGroupBy $ appendExpr expr $ selectClause <> " FROM " <> tableName

addClause :: ToClickhouseQuery a => String -> a -> String
addClause query clause = query <> toClickhouseQuery clause

__limit :: String -> Limit -> String
__limit = addClause

__orderBy :: String -> Order -> String
__orderBy = addClause

__offset :: String -> Offset -> String
__offset = addClause

runClickhouse :: (MonadFlow m, ClickhouseFlow m env, FromJSON a) => (HttpConnection -> IO (Either String a)) -> ClickhouseDb -> m (Either String a)
runClickhouse action db = do
  con <- case db of
    ATLAS_DRIVER_OFFER_BPP -> asks (.driverClickhouseEnv.connection)
    ATLAS_KAFKA -> asks (.kafkaClickhouseEnv.connection)
  L.runIO $ action con

runRawQuery :: (MonadFlow m, ClickhouseFlow m env, FromJSON a) => String -> ClickhouseDb -> m (Either String a)
runRawQuery query db = do
  logDebug $ "clickhouse raw query v1: " <> T.pack query
  runClickhouse (`runQuery` getJSON query) db

runRawQuery' :: (MonadFlow m, ClickhouseFlow m env, FromJSON a) => ClickhouseExpr -> ClickhouseDb -> m (Either String a)
runRawQuery' (ExprStr query) db = runRawQuery query db
runRawQuery' _ _ = throwError $ InternalError "can't call this function with unresolved clickhouseExpr"

constructQuery :: Typeable a => Proxy a -> [Text] -> Maybe String -> ClickhouseExpr -> Maybe Limit -> Maybe Offset -> Maybe Order -> String
constructQuery proxyTable columns maybeGroupBy expr mbLimit mbOffset mbOrder = do
  addOffset . addLimit . addOrder $ __select proxyTable columns maybeGroupBy expr
  where
    addLimit q = maybe q (__limit q) mbLimit
    addOrder q = maybe q (__orderBy q) mbOrder
    addOffset q = maybe q (__offset q) mbOffset

findAll :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => ClickhouseDb -> Proxy a -> [Text] -> Maybe String -> ClickhouseExpr -> Maybe Limit -> Maybe Offset -> Maybe Order -> m (Either String [a])
findAll db proxyTable columns maybeGroupBy expr mbLimit mbOffset mbOrder = runRawQuery (constructQuery proxyTable columns maybeGroupBy expr mbLimit mbOffset mbOrder) db

findOne' :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => ClickhouseDb -> Proxy a -> [Text] -> Maybe String -> ClickhouseExpr -> Maybe Offset -> Maybe Order -> m (Maybe a)
findOne' db proxyTable columns maybeGroupBy expr mbOffset mbOrder = either (\err -> logInfo (T.pack err) $> Nothing) (pure . listToMaybe) =<< runRawQuery (constructQuery proxyTable columns maybeGroupBy expr (Just $ Limit 1) mbOffset mbOrder) db

findOne :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => ClickhouseDb -> Proxy a -> [Text] -> Maybe String -> ClickhouseExpr -> m (Maybe a)
findOne db proxyTable columns maybeGroupBy expr = findOne' db proxyTable columns maybeGroupBy expr Nothing Nothing

findOneWithOrder :: (MonadFlow m, ClickhouseFlow m env, Typeable a, FromJSON a) => ClickhouseDb -> Proxy a -> [Text] -> Maybe String -> ClickhouseExpr -> Order -> m (Maybe a)
findOneWithOrder db proxyTable columns maybeGroupBy expr order = findOne' db proxyTable columns maybeGroupBy expr Nothing (Just order)

-- USAGE EXAPLE
-- res1 :: Either String [Test] <- CH.runRawQuery' $ CH.ExprStr "SELECT * FROM test"
-- res2 :: (Maybe Test) <- CH.findOne (Proxy @Test) ("x" =.= "1")
-- res3 :: (Maybe Test) <- CH.findOneWithOrder (Proxy @Test) ("x" =.= "1") (CH.Desc "x")
-- res4 :: Either String [Test] <- CH.findAll (Proxy @Test) ("x" =.= "1") (Just $ CH.Limit 10) Nothing (Just $ CH.Desc "x")
