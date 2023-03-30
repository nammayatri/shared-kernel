{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module HedisQueries where

import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified Kernel.Storage.Hedis.Config as HedisConfig
import Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common (defaultLoggerConfig)
import Kernel.Types.Flow
import Kernel.Utils.IOLogging
import Test.Tasty
import Test.Tasty.HUnit

type Flow = FlowR FlowEnv

data FlowEnv = FlowEnv
  { hedisEnv :: HedisConfig.HedisEnv,
    loggerEnv :: LoggerEnv
  }

hedisQueryTests :: TestTree
hedisQueryTests =
  withResource
    connectHedis
    cleanAndDisconnectHedis
    hedisQueryTestCases

connectHedis :: IO HedisConfig.HedisEnv
connectHedis =
  HedisConfig.connectHedis HedisConfig.defaultHedisCfg ("hedis_test:" <>)

cleanAndDisconnectHedis :: HedisConfig.HedisEnv -> IO ()
cleanAndDisconnectHedis hedisEnv = do
  runFlow hedisEnv deleteAllTestData
  HedisConfig.disconnectHedis hedisEnv
  where
    deleteAllTestData :: Flow ()
    deleteAllTestData = do
      Hedis.delByPattern "hedis_test:*"

withHedisFlow :: IO HedisConfig.HedisEnv -> Flow Assertion -> Assertion
withHedisFlow hedisEnvIO assertion = do
  hedisEnv <- hedisEnvIO
  join $ runFlow hedisEnv assertion

runFlow :: HedisConfig.HedisEnv -> Flow a -> IO a
runFlow hedisEnv flow =
  R.withFlowRuntime Nothing \flowRt -> do
    withLoggerEnv defaultLoggerConfig Nothing $ \loggerEnv -> do
      runFlowR flowRt (FlowEnv hedisEnv loggerEnv) flow

hedisQueryTestCases :: IO HedisConfig.HedisEnv -> TestTree
hedisQueryTestCases hedisEnv =
  testGroup
    "Hedis Queries Tests"
    [ hedisTestCase "Set test" setTest
    ]
  where
    hedisTestCase :: [Char] -> Flow Assertion -> TestTree
    hedisTestCase label =
      testCase label . withHedisFlow hedisEnv

setTest :: Flow Assertion
setTest = do
  let key = "some_key_1"
      value = "some_value_1" :: Text

  Hedis.set key value
  mResult <- Hedis.get key

  pure $ (Just value) @=? mResult
