{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

-- | Wire types for @cbx-nodal-bulkPaymentInq@ and @cbx-nodal-batchnuminq@.
--
-- HDFC allow a daily maximum of six inquiries per batch number; the budget is spent by the
-- caller, not enforced here. The response repeats every row on every inquiry rather than
-- sending a delta, so applying outcomes must be idempotent.
module Kernel.External.Payout.HdfcCbx.Types.Inquiry where

import Kernel.External.Payout.HdfcCbx.Types.Payment (CdFlag, LenientInt)
import Kernel.Prelude

data CbxInquiryReq = CbxInquiryReq
  { -- | Domain id. Named @gcif@ on both inquiry endpoints and @groupid@ on payment --
    -- confirmed against HDFC's own "Updated Bulk Inquiry Request" sample, which contradicts
    -- the field name in the revised specification sheet.
    gcif :: Text,
    -- | 32
    iduser :: Text,
    -- | 100, from the payment response
    batchnum :: Text,
    -- | 10, DD/MM/YYYY — the date the request was posted
    reqdexctndt :: Text,
    -- | 6; the revised sheet marks it optional, the JOSE guide mandatory. Always send it.
    filerefno :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CbxInquiryResp = CbxInquiryResp
  { gcif :: Maybe Text,
    iduser :: Maybe Text,
    batchnum :: Maybe Text,
    nooftran :: Maybe LenientInt,
    -- | Batch-level accepted or rejected. Not the per-item status.
    codstatus :: Maybe Text,
    message :: Maybe Text,
    filerefno :: Maybe Text,
    trans :: Maybe [CbxInquiryTxn]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CbxInquiryTxn = CbxInquiryTxn
  { cdflag :: Maybe CdFlag,
    -- | Our @custrefno@, echoed. The only reliable way back to a payout order.
    custrefno :: Maybe Text,
    accno :: Maybe Text,
    name :: Maybe Text,
    amount :: Maybe Text,
    ifsc :: Maybe Text,
    micr :: Maybe Text,
    reqdexctndt :: Maybe Text,
    -- | Per-transaction status: @P@ pending approval, @R@ rejected, @C@ completed, @E@ executed.
    -- The meaning of @P@ is contested between the status sheet and the bulk specification.
    codstatus :: Maybe Text,
    -- | Current description, or the rejection reason when @codstatus@ is @R@. 40 chars.
    -- This, not @codstatus@, is what the status map keys on.
    txtreason :: Maybe Text,
    -- | UTR for RTGS and NEFT.
    refno :: Maybe Text,
    -- | FT number for payment type @I@ (intra-bank).
    bankrefno :: Maybe Text,
    -- | Settlement confirmation, only when the RBI status flag is enabled on the domain:
    -- @TXSETT@ settled, @TXSIP@ in progress, @TXREJE@ rejected, @TXDSETT@ deemed settled.
    rbistatus :: Maybe Text,
    -- | Populated only when @rbistatus@ is @TXREJE@.
    rbireason :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | @cbx-nodal-batchnuminq@ — used only when the batch number was not received because
-- the payment request timed out. Keyed on what we wrote before the call, which is why
-- the client reference and value date are persisted at slot-claim time.
data CbxBatchNumReq = CbxBatchNumReq
  { -- | domain id; named @gcif@ on this endpoint and @groupid@ on the others
    gcif :: Text,
    iduser :: Text,
    -- | 10, DD/MM/YYYY
    reqdexctndt :: Text,
    -- | 6, mandatory here
    filerefno :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CbxBatchNumResp = CbxBatchNumResp
  { gcif :: Maybe Text,
    iduser :: Maybe Text,
    filerefno :: Maybe Text,
    -- | Present only on a successful lookup.
    batchnum :: Maybe Text,
    codstatus :: Maybe Text,
    -- | Present only on failure.
    message :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
