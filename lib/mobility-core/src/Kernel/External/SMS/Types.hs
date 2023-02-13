{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SMS.Types
  ( module Kernel.External.SMS.Types,
  )
where

import Data.OpenApi
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)

data SmsService = MyValueFirst | ExotelSms
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

availableSmsServices :: [SmsService]
availableSmsServices = [MyValueFirst, ExotelSms]

derivePersistField "SmsService"
