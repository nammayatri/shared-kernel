module Kernel.Types.GuidLike where

import EulerHS.Prelude
import Kernel.Types.MonadGuid

class GuidLike m a where
  generateGUID :: m a

instance MonadGuid m => GuidLike m Text where
  generateGUID = generateGUIDText
