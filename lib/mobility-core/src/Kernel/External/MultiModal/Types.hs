{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.MultiModal.Types
  ( module Kernel.External.MultiModal.Types,
  )
where

import Data.OpenApi hiding (name)
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Storage.Esqueleto (derivePersistField)

data MultiModalService = GoogleTransit | OTPTransit
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''MultiModalService)

availableMultiModalService :: [MultiModalService]
availableMultiModalService = [GoogleTransit, OTPTransit]

derivePersistField "MultiModalService"
