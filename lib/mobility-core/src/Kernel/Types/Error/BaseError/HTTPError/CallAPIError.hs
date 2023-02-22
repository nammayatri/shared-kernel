 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Error.BaseError.HTTPError.CallAPIError where

import EulerHS.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError (IsBaseException)
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Utils.Error.Throwing
import Servant.Client (ClientError (..))

data CallAPIError err = RawError ClientError | APIError err

extractApiError ::
  FromResponse err =>
  Either ClientError a ->
  Either (CallAPIError err) a
extractApiError = \case
  Left err@(FailureResponse _ response) ->
    Left $ maybe (RawError err) APIError (fromResponse response)
  Left l -> Left (RawError l)
  Right a -> Right a

unwrapEitherCallAPIError ::
  ( MonadThrow m,
    Log m,
    IsBaseException e
  ) =>
  Maybe Text ->
  BaseUrl ->
  (err -> e) ->
  Either (CallAPIError err) a ->
  m a
unwrapEitherCallAPIError errorCodeMb baseUrl toBaseException = fromEitherM' $ \case
  RawError cliErr -> throwError $ ExternalAPICallError errorCodeMb baseUrl cliErr
  APIError err -> throwError (toBaseException err)

unwrapEitherOnlyFromRawError ::
  (MonadThrow m, Log m) =>
  Maybe Text ->
  BaseUrl ->
  Either (CallAPIError err) a ->
  m (Either err a)
unwrapEitherOnlyFromRawError errorCodeMb baseUrl = either left (pure . Right)
  where
    left = \case
      RawError cliErr -> throwError $ ExternalAPICallError errorCodeMb baseUrl cliErr
      APIError err -> pure (Left err)
