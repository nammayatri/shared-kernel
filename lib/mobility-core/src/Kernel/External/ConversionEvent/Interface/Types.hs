module Kernel.External.ConversionEvent.Interface.Types
  ( module Kernel.External.ConversionEvent.Interface.Types,
  )
where

import Kernel.External.ConversionEvent.Meta.Types as Meta
import Kernel.Prelude

data ConversionEventServiceConfig = MetaConfig Meta.MetaConfig
  deriving (Show, FromJSON, ToJSON, Generic, Eq)
