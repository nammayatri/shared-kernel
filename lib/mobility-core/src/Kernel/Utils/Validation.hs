{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Utils.Validation
  ( module Kernel.Utils.Validation,
    module Kernel.Types.Validation,
  )
where

import qualified Data.Either.Validation as V
import Data.Generics.Labels ()
import EulerHS.Prelude hiding (pred)
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Logging
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Utils.Error.Throwing

runRequestValidation ::
  (MonadThrow m, Log m) =>
  Validate obj ->
  obj ->
  m ()
runRequestValidation validator obj =
  V.validationToEither (validator obj)
    & fromEitherM RequestValidationFailure

newtype RequestValidationFailure = RequestValidationFailure [ValidationDescription]
  deriving (Show, IsBaseError, IsBecknAPIError)

instance IsHTTPError RequestValidationFailure where
  toErrorCode (RequestValidationFailure _failures) = "REQUEST_VALIDATION_FAILURE"
  toHttpCode (RequestValidationFailure _failures) = E400

instance IsAPIError RequestValidationFailure where
  toPayload (RequestValidationFailure failures) = toJSON failures

instanceExceptionWithParent 'HTTPException ''RequestValidationFailure

validateField ::
  (Predicate a p, ShowablePredicate p) =>
  Text ->
  a ->
  p ->
  Validation
validateField fieldName fieldValue pred =
  unless (pFun pred fieldValue) . V.Failure $ [validationDescription]
  where
    validationDescription =
      ValidationDescription
        { fieldName = [fieldName],
          expectation = pShow pred fieldName
        }

validateObject ::
  Text ->
  a ->
  Validate a ->
  Validation
validateObject fieldName object validator = addPrefixes fieldName $ validator object

validateList ::
  Container a =>
  Text ->
  a ->
  Validate (Element a) ->
  Validation
validateList fieldName list validator =
  traverse_ f (zip (map (\i -> fieldName <> "[" <> show i <> "]") [0 :: Int ..]) $ toList list)
  where
    f (pref, val) = addPrefixes pref $ validator val

addPrefixes :: Text -> Validation -> Validation
addPrefixes fieldName = first $ map (addPrefixToFieldName fieldName)

addPrefixToFieldName ::
  Text ->
  ValidationDescription ->
  ValidationDescription
addPrefixToFieldName prefix = #fieldName %~ (prefix :)
