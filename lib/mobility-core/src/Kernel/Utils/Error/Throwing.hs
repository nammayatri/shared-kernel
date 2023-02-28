{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Error.Throwing
  ( module Kernel.Utils.Error.Throwing,
    IsHTTPException,
  )
where

import Control.Exception (PatternMatchFail)
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Logging

throwError :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> m b
throwError exc = do
  let someExc = toException exc
  logWarning $ makeLogSomeException someExc
  logCallStack
  throwM someExc
  where
    logCallStack =
      withLogTag "CallStack" $
        logDebug . T.pack $ prettyCallStack callStack

fromMaybeM ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> Maybe b -> m b
fromMaybeM err = maybe (throwError err) pure

fromEitherM ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => (left -> e) -> Either left b -> m b
fromEitherM toerr = fromEitherM' (throwError . toerr)

liftEither ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => Either e b -> m b
liftEither = fromEitherM id

fromEitherM' ::
  Applicative m => (l -> m r) -> Either l r -> m r
fromEitherM' f = either f pure

safeCatch ::
  ( HasCallStack,
    MonadCatch m,
    Log m,
    Exception e
  ) =>
  m a ->
  (e -> m a) ->
  m a
safeCatch m f =
  m `catch` \exc ->
    f exc `catch` \(_ :: PatternMatchFail) ->
      throwM exc

-- Given
--   data E = A | B
-- an action
--   throw B `safeCatch` \A -> ...
-- does not fail with "Non-exhaustive patterns"

rethrow ::
  ( HasCallStack,
    MonadCatch m,
    Log m,
    Exception e,
    IsBaseException e'
  ) =>
  m a ->
  (e -> e') ->
  m a
rethrow m f = m `safeCatch` (throwError . f)
