{-# OPTIONS_GHC -Wno-orphans #-}

module Version where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Version
import Test.Tasty
import Test.Tasty.HUnit

instance Log (Either SomeException) where
  logOutput _logLevel _msg = pure ()
  withLogTag _ a = a

successfull :: TestTree
successfull =
  testCase "Successfull version reading" $
    successfullRead @?= "1.0.1"

failed :: TestTree
failed =
  testCase "Failed version reading" $
    failRead @?= "something wrong"

successfullRead :: Text
successfullRead = case readVersion "1.0.1" of
  Left _ -> "something wrong"
  Right v -> versionToText v

failRead :: Text
failRead = case readVersion "1.01" of
  Left _ -> "something wrong"
  Right v -> versionToText v

readVersionTests :: TestTree
readVersionTests =
  testGroup
    "Read versions tests"
    [ successfull,
      failed
    ]
