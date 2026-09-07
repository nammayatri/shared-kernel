{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

-- | Wire types for @cbx-nodal-bulkPayment@.
--
-- Field names match HDFC's JSON exactly and are lower-case by their convention, not ours.
-- Widths in comments are from BulkAPI_Specifications_APISpecs_Revised.xlsx and are enforced
-- by the adapter before serialisation, since HDFC reject the whole file on a single overflow.
module Kernel.External.Payout.HdfcCbx.Types.Payment where

import Data.Aeson (Value (..), withText)
import Data.Aeson.Types (typeMismatch)
import Kernel.Prelude

-- | Transaction type. HDFC have no UPI rail: every bulk payment is account-and-IFSC based.
data CdFlag
  = -- | @I@ — intra-bank; rejected at processing if the IFSC is not HDFC
    A2A
  | -- | @N@
    NEFT
  | -- | @R@
    RTGS
  | -- | @M@
    IMPS
  deriving stock (Show, Read, Eq, Ord, Generic)

cdFlagCode :: CdFlag -> Text
cdFlagCode = \case
  A2A -> "I"
  NEFT -> "N"
  RTGS -> "R"
  IMPS -> "M"

instance ToJSON CdFlag where
  toJSON = toJSON . cdFlagCode

instance FromJSON CdFlag where
  parseJSON = withText "CdFlag" $ \case
    "I" -> pure A2A
    "N" -> pure NEFT
    "R" -> pure RTGS
    "M" -> pure IMPS
    other -> fail $ "unsupported cdflag: " <> show other

data CbxPaymentReq = CbxPaymentReq
  { -- | 40, from CBX setup
    clientcode :: Text,
    -- | 50, domain id
    groupid :: Text,
    -- | 32, maker id
    iduser :: Text,
    -- | 3 digits, <= 500, must equal @length trans@
    -- | 3 digits, <= 500, must equal @length trans@. Emitted as a JSON *string*: HDFC's
    -- own updated request sample has @"nooftran": "1"@ while their acknowledgement echoes
    -- it back as a number, so 'LenientInt' accepts either and writes the string form.
    nooftran :: LenientInt,
    -- | 6 numeric, unique per request
    filerefno :: Text,
    trans :: [CbxPaymentTxn]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CbxPaymentTxn = CbxPaymentTxn
  { cdflag :: CdFlag,
    -- | 25, beneficiary account number
    accno :: Text,
    -- | 11
    ifsc :: Text,
    -- | 20 (17.2); "123.00" | "123" | "123.0" — anything else is invalid
    amount :: Text,
    -- | 200, but max 40 on NEFT and RTGS
    name :: Text,
    -- | 20, echoed back on every inquiry row; our join key
    custrefno :: Text,
    -- | 10, DD/MM/YYYY
    reqdexctndt :: Text,
    -- | 35, beneficiary code; needed only if registration is mandatory
    code :: Maybe Text,
    -- | 100, derivable from IFSC
    bankname :: Maybe Text,
    -- | 40
    branch :: Maybe Text,
    -- | 100, advice for RBI payments
    email :: Maybe Text,
    -- | 20
    instrefno :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Synchronous acknowledgement. Per test case 203 a valid file lands in a checker's queue,
-- so an accepted response means /queued for approval/, not /accepted for payment/.
data CbxPaymentResp = CbxPaymentResp
  { clientcode :: Maybe Text,
    groupid :: Maybe Text,
    iduser :: Maybe Text,
    -- | Human-readable outcome. @"ACCEPTED"@ on success; on a negative acknowledgement this
    -- carries the reason, e.g. @"Sorry, this is a duplicate transaction request"@.
    txtstatus :: Maybe Text,
    -- | Bank-generated batch reference, @<groupid><clientcode><timestamp>@.
    -- Empty string (not null) on a negative acknowledgement.
    batchnum :: Maybe Text,
    nooftran :: Maybe LenientInt,
    filerefno :: Maybe Text,
    -- | @"0"@ accepted, @"1"@ rejected.
    codstatus :: Maybe Text,
    message :: Maybe Text,
    -- | The full request is echoed back on both acknowledgement and rejection.
    trans :: Maybe [Value]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | HDFC are inconsistent about whether @nooftran@ is a JSON string or a number, even
-- between the request and its own acknowledgement. Accept either; emit a string.
newtype LenientInt = LenientInt {getLenientInt :: Int}
  deriving stock (Show, Eq, Generic)

instance FromJSON LenientInt where
  parseJSON (Number n) = pure . LenientInt . truncate $ n
  parseJSON (String t) = maybe (fail $ "nooftran not numeric: " <> show t) (pure . LenientInt) (readMaybe (toString t))
  parseJSON v = typeMismatch "LenientInt" v

instance ToJSON LenientInt where
  toJSON = toJSON . show @Text . getLenientInt
