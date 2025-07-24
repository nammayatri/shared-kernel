module Kernel.External.ConversionEvent.Types
  ( module Kernel.External.ConversionEvent.Types,
  )
where

import Data.Text
import Kernel.Prelude

data ConversionEventService = Meta
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

data ConversionReqType = ConversionReqType
  { eventName :: Text
  }
