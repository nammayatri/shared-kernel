{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.BackgroundVerification.Types
  ( module Reexport,
    module Kernel.External.BackgroundVerification.Types,
  )
where

import Data.Aeson.Types
import EulerHS.Prelude
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.External.BackgroundVerification.Checkr.Config as Reexport
import Kernel.Storage.Esqueleto (derivePersistField)

newtype BackgroundVerificationServiceConfig = CheckrConfig CheckrCfg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data BackgroundVerificationService = Checkr
  deriving (Show, Read, Eq, Ord, Generic)

$(mkBeamInstancesForEnumAndList ''BackgroundVerificationService)
derivePersistField "BackgroundVerificationService"

-- Generic instances for type with single value will not work
instance FromJSON BackgroundVerificationService where
  parseJSON (String "Checkr") = pure Checkr
  parseJSON (String _) = parseFail "Expected \"Checkr\""
  parseJSON e = typeMismatch "String" e

instance ToJSON BackgroundVerificationService where
  toJSON = String . show
