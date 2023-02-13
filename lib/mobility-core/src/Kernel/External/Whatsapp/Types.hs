{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Whatsapp.Types
  ( module Kernel.External.Whatsapp.Types,
  )
where

import Kernel.Storage.Esqueleto (derivePersistField)
import Data.OpenApi
import EulerHS.Prelude

data WhatsappService = GupShup
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

availableSmsServices :: [WhatsappService]
availableSmsServices = [GupShup]

derivePersistField "WhatsappService"
