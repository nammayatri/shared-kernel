{-# LANGUAGE UndecidableInstances #-}

module Kernel.Types.BecknRequest where

import Kernel.Types.Id (Id)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)

data BecknRequest = BecknRequest
  { id :: Id BecknRequest,
    becknRequest :: Text,
    signatureHeader :: Text,
    timeStamp :: UTCTime
  }
  deriving (Generic)
