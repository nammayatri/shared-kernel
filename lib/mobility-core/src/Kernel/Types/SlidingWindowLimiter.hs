module Kernel.Types.SlidingWindowLimiter where

import Kernel.Utils.Dhall (FromDhall)
import EulerHS.Prelude

data APIRateLimitOptions = APIRateLimitOptions
  { limit :: Int,
    limitResetTimeInSec :: Int
  }
  deriving (Generic, FromDhall)
