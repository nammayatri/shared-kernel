{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

 distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

 FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

 General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

-- | Shared types for the Parivahan ntrsearchservice (MoRTH v2.1).
module Kernel.External.Verification.MorthV2.Types where

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Kernel.External.Encryption
import Kernel.Prelude

-- | Configuration for the v2.1 (ntrsearchservice) provider.
--
-- Unlike v1, @apiKey@ is NOT sent as a header. It is the AES-GCM password
-- used by @Kernel.External.Verification.MorthV2.Crypto@.
data MorthV2VerificationCfg = MorthV2VerificationCfg
  { url :: BaseUrl,
    -- | Issued by the Parivahan portal; identifies the API consumer. Carried
    -- plaintext in the envelope and must match the inner @userId@ and the JWT
    -- subject on every call.
    clientId :: Text,
    -- | AES-GCM password (PBKDF2 input).
    apiKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Envelope: the wire shape used by every request and every success response.
-- ---------------------------------------------------------------------------

data Envelope = Envelope
  { clientId :: Text,
    encData :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Shape of plain (non-encrypted) auth/envelope errors returned by the
-- server before decryption is attempted — see section 4.1 of the API docs.
data PlainError = PlainError
  { status :: Value, -- Server inconsistently returns Int or String; keep raw.
    message :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Token API — POST /api/auth/token
-- ---------------------------------------------------------------------------

newtype TokenReqPayload = TokenReqPayload {clientId :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TokenRespPayload = TokenRespPayload
  { token :: Text,
    tokenType :: Text,
    expiresInMs :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Vehicle lookup — POST /api/getVehicleDetailsByRegnNo
-- ---------------------------------------------------------------------------

data VehicleByRegnReqPayload = VehicleByRegnReqPayload
  { userId :: Text,
    regnNo :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Vehicle lookup — POST /api/getVehicleDetailsByChasiNoAndEngNo
-- Field name is @chasiNo@ (single 's'), per the v2.1 spec note.
-- ---------------------------------------------------------------------------

data VehicleByChasiEngReqPayload = VehicleByChasiEngReqPayload
  { userId :: Text,
    chasiNo :: Text,
    engNo :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Common response shape for both vehicle lookup endpoints. @vehicleData@
-- is a @Map@ so unknown server-side keys don't break parsing.
data VehicleDetailsResp = VehicleDetailsResp
  { success :: Maybe Bool,
    registrationNumber :: Maybe Text,
    userId :: Maybe Text,
    consentStatus :: Maybe Text,
    consentRequested :: Maybe Bool,
    totalFieldsAllowed :: Maybe Int,
    totalFieldsReturned :: Maybe Int,
    maskedFieldsCount :: Maybe Int,
    unmaskedFieldsCount :: Maybe Int,
    fieldsMasked :: Maybe [Text],
    fieldsNotMasked :: Maybe [Text],
    vehicleData :: Maybe (Map Text Text),
    timestamp :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Convenience lookup into @vehicleData@. Masked values (e.g. @\"AC**VE\"@)
-- are returned as-is; callers decide whether to strip them.
lookupVehicleField :: Text -> VehicleDetailsResp -> Maybe Text
lookupVehicleField k r = r.vehicleData >>= Map.lookup k

-- ---------------------------------------------------------------------------
-- Driving License — POST /api/getLicenseDetails
-- ---------------------------------------------------------------------------

data LicenseDetailsReqPayload = LicenseDetailsReqPayload
  { dlNumber :: Text,
    dateOfBirth :: Text, -- "YYYY-MM-DD"
    userId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Decoded response for @/api/getLicenseDetails@. @drivingLicenseData@ is
-- a @Map@ so unknown server-side keys don't break parsing.
data LicenseDetailsResp = LicenseDetailsResp
  { success :: Maybe Bool,
    drivingLicenseNumber :: Maybe Text,
    userId :: Maybe Text,
    totalFieldsAllowed :: Maybe Int,
    totalFieldsReturned :: Maybe Int,
    maskedFieldsCount :: Maybe Int,
    unmaskedFieldsCount :: Maybe Int,
    fieldsMasked :: Maybe [Text],
    fieldsNotMasked :: Maybe [Text],
    drivingLicenseData :: Maybe (Map Text Text),
    timestamp :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

lookupLicenseField :: Text -> LicenseDetailsResp -> Maybe Text
lookupLicenseField k r = r.drivingLicenseData >>= Map.lookup k
