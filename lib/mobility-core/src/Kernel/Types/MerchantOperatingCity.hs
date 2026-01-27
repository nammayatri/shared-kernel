{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Kernel.Types.MerchantOperatingCity where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Id

data MerchantOperatingCity = MerchantOperatingCity
  { id :: Id MerchantOperatingCity,
    city :: Text,
    stdCode :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
