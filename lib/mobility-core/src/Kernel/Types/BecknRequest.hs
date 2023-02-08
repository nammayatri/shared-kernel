{-# LANGUAGE UndecidableInstances #-}

module Kernel.Types.BecknRequest where

import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id (Id)

data BecknRequest = BecknRequest
  { id :: Id BecknRequest,
    becknRequest :: Text,
    signatureHeader :: Text,
    timeStamp :: UTCTime
  }
  deriving (Generic)
