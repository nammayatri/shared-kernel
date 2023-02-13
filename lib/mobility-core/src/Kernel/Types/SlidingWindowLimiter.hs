module Kernel.Types.SlidingWindowLimiter where

import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)

data APIRateLimitOptions = APIRateLimitOptions
  { limit :: Int,
    limitResetTimeInSec :: Int
  }
  deriving (Generic, FromDhall)
