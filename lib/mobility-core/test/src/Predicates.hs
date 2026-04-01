{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Predicates where

import qualified Data.Either.Validation as V
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Test.Tasty
import Test.Tasty.HUnit

predicatesTests :: TestTree
predicatesTests =
  testGroup
    "Kernel.Utils.Predicates"
    [ namePredicateTests,
      nameValidateFieldTests
    ]

namePredicateTests :: TestTree
namePredicateTests =
  testGroup
    "P.name (Text)"
    [ testGroup
        "valid"
        $ map
          (uncurry validCase)
          [ ("Mohammed", "Mohammed"),
            ("Al-GBURI", "Al-GBURI"),
            ("O'Connor", "O'Connor"),
            ("Jean Paul", "Jean Paul"),
            ("Jean-Paul", "Jean-Paul"),
            ("D'Arcy", "D'Arcy"),
            ("A B", "A B"),
            ("A-B-C", "A-B-C")
          ],
      testGroup
        "invalid"
        $ map
          (uncurry invalidCase)
          [ ("empty", ""),
            ("only space", " "),
            ("leading hyphen", "-A"),
            ("trailing hyphen", "A-"),
            ("double hyphen", "A--B"),
            ("double apostrophe", "A''B"),
            ("double space", "A  B"),
            ("underscore", "A_B"),
            ("dot", "A.B"),
            ("digit", "A1"),
            ("trailing space", "Al-GBURI "),
            ("leading space", " Al")
          ]
    ]
  where
    validCase :: Text -> Text -> TestTree
    validCase label t =
      testCase (T.unpack label) $
        assertBool ("expected valid: " <> T.unpack label) (pFun P.name t)
    invalidCase :: Text -> Text -> TestTree
    invalidCase label t =
      testCase (T.unpack label) $
        assertBool ("expected invalid: " <> T.unpack label) (not (pFun P.name t))

nameValidateFieldTests :: TestTree
nameValidateFieldTests =
  testGroup
    "P.name via validateField / InMaybe"
    [ testCase "validateField accepts valid Text" $
        case validateField "fullName" ("Jean-Paul" :: Text) P.name of
          V.Success () -> pure ()
          V.Failure errs -> assertFailure $ "unexpected failure: " <> show errs,
      testCase "validateField rejects invalid Text" $
        case validateField "fullName" ("A--B" :: Text) P.name of
          V.Success () -> assertFailure "expected validation failure"
          V.Failure [ValidationDescription {fieldName = ["fullName"], expectation}] ->
            expectation @=? pShow P.name "fullName"
          V.Failure errs -> assertFailure $ "unexpected failures: " <> show errs,
      testCase "InMaybe: Nothing passes" $
        case validateField "fullName" (Nothing :: Maybe Text) (InMaybe P.name) of
          V.Success () -> pure ()
          V.Failure errs -> assertFailure $ "unexpected failure: " <> show errs,
      testCase "InMaybe: Just valid passes" $
        case validateField "fullName" (Just ("O'Connor" :: Text)) (InMaybe P.name) of
          V.Success () -> pure ()
          V.Failure errs -> assertFailure $ "unexpected failure: " <> show errs,
      testCase "InMaybe: Just invalid fails" $
        case validateField "fullName" (Just ("A1" :: Text)) (InMaybe P.name) of
          V.Success () -> assertFailure "expected validation failure"
          V.Failure [ValidationDescription {fieldName = ["fullName"], expectation}] ->
            expectation @=? pShow (InMaybe P.name) "fullName"
          V.Failure errs -> assertFailure $ "unexpected failures: " <> show errs
    ]
