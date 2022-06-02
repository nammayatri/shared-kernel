module Beckn.Product.Validation.Context where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Cab
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Cab.Action -> Cab.Context -> m ()
validateContext action context = do
  validateDomain Cab.MOBILITY context
  validateContextCommons action context

validateDomain :: (L.MonadFlow m, Log m) => Cab.Domain -> Cab.Context -> m ()
validateDomain expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateCountry :: (L.MonadFlow m, Log m) => Cab.Context -> m ()
validateCountry context =
  unless (context.country == "IND") $
    throwError InvalidCountry

validateAction :: (L.MonadFlow m, Log m) => Cab.Action -> Cab.Context -> m ()
validateAction expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Cab.Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context.core_version == supportedVersion) $
    throwError UnsupportedCoreVer

validateContextCommons ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Cab.Action ->
  Cab.Context ->
  m ()
validateContextCommons expectedAction context = do
  -- TODO: City validation
  validateAction expectedAction context
  validateCoreVersion context
  validateCountry context
