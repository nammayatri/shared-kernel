module Kernel.Types.Schema where

import EulerHS.Prelude

class HasSchemaName m where
  getSchemaName :: m Text
