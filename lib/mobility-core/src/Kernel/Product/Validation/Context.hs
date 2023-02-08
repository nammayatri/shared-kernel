module Kernel.Product.Validation.Context where

import Kernel.Types.Common
import qualified Kernel.Types.Beckn.Context as CoreContext
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateContext :: (HasFlowEnv m r '["coreVersion" ::: Text]) => CoreContext.Action -> CoreContext.Context -> m ()
validateContext action context = do
  validateDomain CoreContext.MOBILITY context
  validateContextCommons action context

validateMetroContext :: (HasFlowEnv m r '["coreVersion" ::: Text]) => CoreContext.Action -> CoreContext.Context -> m ()
validateMetroContext action context = do
  validateDomain CoreContext.METRO context
  validateContextCommons action context

validateDomain :: (L.MonadFlow m, Log m) => CoreContext.Domain -> CoreContext.Context -> m ()
validateDomain expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateCountry :: (L.MonadFlow m, Log m) => CoreContext.Context -> m ()
validateCountry context =
  unless (context.country == "IND") $
    throwError InvalidCountry

validateAction :: (L.MonadFlow m, Log m) => CoreContext.Action -> CoreContext.Context -> m ()
validateAction expectedAction context =
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
  validateCountry context
