{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Verification.Types
  ( module Kernel.External.Verification.Types,
  )
where

import Data.Aeson as A
import Data.OpenApi hiding (email)
import qualified Data.Text as T
import EulerHS.Prelude hiding (state)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import qualified Kernel.Prelude as KP
import Kernel.Storage.Esqueleto (derivePersistField)

data VerificationService = Idfy | InternalScripts | GovtData | HyperVerge | HyperVergeRCDL | DigiLocker | Tten
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data DriverBackgroundVerificationService = SafetyPortal
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, ToSchema)

instance FromJSON DriverBackgroundVerificationService where -- remove this instance once you add more constructors to AadhaarVerificationService type.
  parseJSON (A.String val) = maybe (fail ("failed to parse String " <> show val <> " in DriverBackgroundVerificationService type")) pure (KP.readMaybe $ T.unpack val)
  parseJSON (A.Array _) = pure SafetyPortal
  parseJSON e = fail $ "unexpected type, expected String for AadhaarVerificationService" <> show e

$(mkBeamInstancesForEnumAndList ''VerificationService)
$(mkBeamInstancesForEnumAndList ''DriverBackgroundVerificationService)

availableVerificationServices :: [VerificationService]
availableVerificationServices = [Idfy, InternalScripts]

availableDriverBackgroundVerificationServices :: [DriverBackgroundVerificationService]
availableDriverBackgroundVerificationServices = [SafetyPortal]

derivePersistField "VerificationService"
derivePersistField "DriverBackgroundVerificationService"

data RCVerificationResponse = RCVerificationResponse
  { registrationDate :: Maybe Text,
    registrationNumber :: Maybe Text,
    fitnessUpto :: Maybe Text,
    insuranceValidity :: Maybe Text,
    vehicleClass :: Maybe Text,
    vehicleCategory :: Maybe Text,
    seatingCapacity :: Maybe A.Value,
    manufacturer :: Maybe Text,
    permitValidityFrom :: Maybe Text,
    permitValidityUpto :: Maybe Text,
    pucValidityUpto :: Maybe Text,
    manufacturerModel :: Maybe Text,
    mYManufacturing :: Maybe Text,
    color :: Maybe Text,
    fuelType :: Maybe Text,
    bodyType :: Maybe Text,
    status :: Maybe Text,
    grossVehicleWeight :: Maybe Float,
    unladdenWeight :: Maybe Float
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data PanInputDetails = PanInputDetails
  { inputPanNumber :: Text,
    inputName :: Maybe Text,
    inputDob :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data PanVerificationResponse = PanVerificationResponse
  { aadhaarSeedingStatus :: Maybe Bool,
    panStatus :: Maybe Text,
    nameMatch :: Maybe Bool,
    dobMatch :: Maybe Bool,
    inputDetails :: Maybe PanInputDetails,
    status :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data GstVerificationResponse = GstVerificationResponse
  { additionalPlaceOfBusinessFields :: Maybe A.Value,
    centreJurisdiction :: Maybe Text,
    centreJurisdictionCode :: Maybe Text,
    constitutionOfBusiness :: Maybe Text,
    dateOfCancellation :: Maybe Text,
    dateOfRegistration :: Maybe Text,
    gstin :: Maybe Text,
    gstinStatus :: Maybe Text,
    lastUpdatedDate :: Maybe Text,
    legalName :: Maybe Text,
    natureOfBusinessActivity :: Maybe A.Value,
    principalPlaceOfBusinessFields :: Maybe A.Value,
    source :: Maybe Text,
    stateJurisdictionCode :: Maybe Text,
    status :: Maybe Text,
    taxpayerType :: Maybe Text,
    tradeName :: Maybe Text,
    einvoiceStatus :: Maybe Text,
    statusDetails :: Maybe Text,
    isSez :: Maybe Text,
    filingDetails :: Maybe A.Value
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data UdyogAadhaarVerificationResponse = UdyogAadhaarVerificationResponse
  { udyogAadhaarNumber :: Maybe Text,
    enterpriseName :: Maybe Text,
    enterpriseType :: Maybe Text,
    majorActivity :: Maybe Text,
    socialCategory :: Maybe Text,
    commencementDate :: Maybe Text,
    dicName :: Maybe Text,
    state :: Maybe Text,
    appliedDate :: Maybe Text,
    expiryDate :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data UdyamAadhaarVerificationResponse = UdyamAadhaarVerificationResponse
  { udyamAadhaarNumber :: Maybe Text,
    enterpriseName :: Maybe Text,
    enterpriseType :: Maybe Text,
    majorActivity :: Maybe Text,
    socialCategory :: Maybe Text,
    commencementDate :: Maybe Text,
    dicName :: Maybe Text,
    state :: Maybe Text,
    appliedDate :: Maybe Text,
    dateOfInc :: Maybe Text,
    msmeDi :: Maybe Text,
    organizationType :: Maybe Text,
    address :: Maybe Text,
    email :: Maybe Text,
    mobile :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data BankAccountVerificationResponse = BankAccountVerificationResponse
  { accountExists :: Bool,
    amountDeposited :: Maybe Text,
    bankAccountNumber :: Maybe Text,
    ifscCode :: Maybe Text,
    message :: Maybe Text,
    nameAtBank :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

data PanAadhaarLinkResponse = PanAadhaarLinkResponse
  { isLinked :: Maybe Bool,
    message :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)
