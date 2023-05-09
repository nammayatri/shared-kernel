module Kernel.Connection.EnvVars where

import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)

postgresConnectionName :: IO Text
postgresConnectionName = (T.pack . (fromMaybe "postgres") <$> lookupEnv "POSTGRES_CONNECTION_NAME")

getMysqlPoolStripes :: IO Int
getMysqlPoolStripes = ((fromMaybe 1) . (readMaybe =<<)) <$> lookupEnv "MYSQL_POOL_STRIPES"

getMysqlPoolIdleTime :: IO Integer
getMysqlPoolIdleTime = (fromMaybe 20000 . (readMaybe =<<)) <$> lookupEnv "MYSQL_POOL_IDLE_TIME"

getMysqlPoolMax :: IO Int
getMysqlPoolMax = (fromMaybe 5 . (readMaybe =<<)) <$> lookupEnv "MYSQL_POOL_MAX"

toTitle :: String -> String
toTitle "" = ""
toTitle (x : xs) = toUpper x : map toLower xs

getPreparePosgreSqlConnection :: IO Bool
getPreparePosgreSqlConnection = do
  envVal <- lookupEnv "PREPARE_POSGRESQL_CONNECTION"
  pure $ fromMaybe True (readMaybe . toTitle =<< envVal)
