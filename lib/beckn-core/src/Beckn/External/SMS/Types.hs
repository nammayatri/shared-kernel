{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.External.SMS.Types
  ( module Beckn.External.SMS.Types,
  )
where

import Beckn.Storage.Esqueleto (derivePersistField)
import Data.OpenApi
import EulerHS.Prelude

data SmsService = MyValueFirst | ExotelSms
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

derivePersistField "SmsService"