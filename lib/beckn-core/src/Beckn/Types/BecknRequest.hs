{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.BecknRequest where

import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)

data BecknRequest = BecknRequest
  { id :: Id BecknRequest,
    becknRequest :: Text,
    signatureHeader :: Text,
    timeStamp :: UTCTime
  }
  deriving (Generic)
