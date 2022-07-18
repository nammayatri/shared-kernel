module Beckn.Product.Validation.Context where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as CoreContext
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => CoreContext.Action -> CoreContext.Context -> m ()
validateContext action context = do
  validateDomain CoreContext.MOBILITY context
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
  unless ((context.core_version == supportedVersion) || (context.domain == CoreContext.LOGISTICS)) $
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
