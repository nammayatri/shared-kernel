{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Product.Validation.Context where

import Control.Lens (view)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (view)
import qualified Kernel.Types.Beckn.Context as CoreContext
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Utils.Common

validateContext :: (HasFlowEnv m r '["coreVersion" ::: Text]) => CoreContext.Action -> CoreContext.Context -> m ()
validateContext action context = do
  validateDomain CoreContext.MOBILITY context
  validateContextCommons action context

validateContextV2 :: (HasFlowEnv m r '["_version" ::: Text]) => CoreContext.Action -> CoreContext.ContextV2 -> m ()
validateContextV2 action context = do
  validateDomainV2 CoreContext.MOBILITY context
  validateContextCommonsV2 action context

validateMetroContext :: (HasFlowEnv m r '["coreVersion" ::: Text]) => CoreContext.Action -> CoreContext.Context -> m ()
validateMetroContext action context = do
  validateDomain CoreContext.METRO context
  validateContextCommons action context

validateDomain :: (L.MonadFlow m, Log m) => CoreContext.Domain -> CoreContext.Context -> m ()
validateDomain expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateDomainV2 :: (L.MonadFlow m, Log m) => CoreContext.Domain -> CoreContext.ContextV2 -> m ()
validateDomainV2 expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateAction :: (L.MonadFlow m, Log m) => CoreContext.Action -> CoreContext.Context -> m ()
validateAction expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateActionV2 :: (L.MonadFlow m, Log m) => CoreContext.Action -> CoreContext.ContextV2 -> m ()
validateActionV2 expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  CoreContext.Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context.core_version == supportedVersion) $
    throwError UnsupportedCoreVer

validateCoreVersionV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    Log m
  ) =>
  CoreContext.ContextV2 ->
  m ()
validateCoreVersionV2 context = do
  supportedVersion <- view #_version
  unless (context._version == supportedVersion) $
    throwError UnsupportedCoreVer

validateContextCommons ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  CoreContext.Action ->
  CoreContext.Context ->
  m ()
validateContextCommons expectedAction context = do
  -- TODO: City validation
  validateAction expectedAction context
  validateCoreVersion context

validateContextCommonsV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    Log m
  ) =>
  CoreContext.Action ->
  CoreContext.ContextV2 ->
  m ()
validateContextCommonsV2 expectedAction context = do
  -- TODO: City validation
  validateActionV2 expectedAction context
  validateCoreVersionV2 context
