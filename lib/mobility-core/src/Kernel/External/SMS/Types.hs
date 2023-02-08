{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SMS.Types
  ( module Kernel.External.SMS.Types,
  )
where

import Kernel.Storage.Esqueleto (derivePersistField)
import Data.OpenApi
import EulerHS.Prelude

data SmsService = MyValueFirst | ExotelSms
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

availableSmsServices :: [SmsService]
availableSmsServices = [MyValueFirst, ExotelSms]

derivePersistField "SmsService"
