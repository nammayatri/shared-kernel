{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.ChallanSearch.Types
  ( module Kernel.External.ChallanSearch.Types,
  )
where

import Data.OpenApi hiding (email)
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)

data ChallanSearchService = Signzy
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''ChallanSearchService)
