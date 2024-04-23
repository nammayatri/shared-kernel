{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Version where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Version
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
    [ successfull
    ]
