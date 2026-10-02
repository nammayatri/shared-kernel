{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

-- | Wire types for @cbx-nodal-bulkPaymentInq@ and @cbx-getBatchNo@ (the API portal's v2 name
-- for what HDFC's bulk spec sheet calls @cbx-nodal-batchnuminq@).
--
-- HDFC allow a daily maximum of six inquiries per batch number; the budget is spent by the
-- caller, not enforced here. The response repeats every row on every inquiry rather than
-- sending a delta, so applying outcomes must be idempotent.
module Kernel.External.Payout.HdfcCbx.Types.Inquiry where

import qualified Data.Aeson as A
import qualified Data.Text as T
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
  deriving anyclass (FromJSON)

-- | Hand-written for wire order (generic encoding alphabetises); the sample's order is
-- gcif, iduser, batchnum, reqdexctndt, filerefno. An absent filerefno goes as @""@, never
-- @null@ -- no HDFC sample anywhere carries a null.
instance ToJSON CbxInquiryReq where
  toJSON = A.object . inquiryPairs
  toEncoding = A.pairs . mconcat . inquiryPairs

inquiryPairs :: (A.KeyValue kv) => CbxInquiryReq -> [kv]
inquiryPairs r =
  [ "gcif" A..= r.gcif,
    "iduser" A..= r.iduser,
    "batchnum" A..= r.batchnum,
    "reqdexctndt" A..= r.reqdexctndt,
    "filerefno" A..= fromMaybe "" r.filerefno
  ]

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

-- | Per-transaction processing status, @codstatus@ on an inquiry row.
--
-- Four values, per @BulkAPI_Specifications_APISpecs_Revised.xlsx@ sheet @Inquiry_Resp@ field 12.
-- The API-portal PDF still documents three and glosses @P@ as \"Processing\"; the revised sheet
-- is authoritative -- it is the only source that accounts for the @E@ in HDFC's own sample
-- response, and the consolidated test cases confirm @P@ by describing the row sitting in the
-- checker's queue with status \"Pending Approval\".
--
-- Parsed into a type rather than matched as text so that every consumer is total over the set
-- and an unrecognised code cannot be silently read as success or failure.
-- The specification defines exactly four codstatus values: P/C/R/E. codstatus is the debit axis --
-- whether money left our nodal account -- while settlement (whether the beneficiary was credited)
-- is a second axis, 'CbxRbiStatus'.
data CbxTxnStatus
  = -- | @P@ -- \"Pending Approval\". Waiting for a checker to approve the batch on HDFC's portal.
    -- A human step, so it can persist for a long time and is not evidence of a problem.
    CbxPendingApproval
  | -- | @R@ -- \"Rejected\". @txtreason@ carries why. Terminal failure.
    CbxRejected
  | -- | @C@ -- \"In Process\". Being processed / pending authorisation on HDFC's side; not
    -- terminal. Keep inquiring.
    CbxInProcess
  | -- | @E@ -- \"Completed\". Debited from our nodal account. For an intra-bank transfer this is
    -- the confirmation the beneficiary was credited (there is no RBI settlement layer); for
    -- NEFT/RTGS/IMPS it only means debited -- beneficiary credit is confirmed by @rbistatus@
    -- ('CbxRbiStatus').
    CbxCompleted
  | -- | Anything else. Not expected -- the specification defines only P/C/R/E -- so it is held as
    -- interim and surfaced for alerting, never read as success or failure.
    CbxUnknownStatus Text
  deriving stock (Show, Eq, Generic)

parseCbxTxnStatus :: Maybe Text -> CbxTxnStatus
parseCbxTxnStatus mbRaw = case T.toUpper (T.strip (fromMaybe "" mbRaw)) of
  "P" -> CbxPendingApproval
  "R" -> CbxRejected
  "C" -> CbxInProcess
  "E" -> CbxCompleted
  other -> CbxUnknownStatus other

-- | Settlement confirmation, @rbistatus@ on an inquiry row. A second axis over 'CbxTxnStatus',
-- present only when the RBI status flag is enabled on the CBX domain -- so its absence carries
-- no information and must never be read as a failure.
data CbxRbiStatus
  = -- | @TXSETT@
    CbxSettled
  | -- | @TXSIP@ -- settlement in progress.
    CbxSettlementInProgress
  | -- | @TXREJE@ -- returned after the debit. @rbireason@ is populated only for this value.
    CbxSettlementRejected
  | -- | @TXDSETT@ -- deemed settled.
    CbxDeemedSettled
  | CbxUnknownRbiStatus Text
  deriving stock (Show, Eq, Generic)

parseCbxRbiStatus :: Maybe Text -> Maybe CbxRbiStatus
parseCbxRbiStatus mbRaw = case T.toUpper (T.strip (fromMaybe "" mbRaw)) of
  "" -> Nothing
  "TXSETT" -> Just CbxSettled
  "TXSIP" -> Just CbxSettlementInProgress
  "TXREJE" -> Just CbxSettlementRejected
  "TXDSETT" -> Just CbxDeemedSettled
  other -> Just (CbxUnknownRbiStatus other)

-- | @cbx-getBatchNo@ (v2 on the API portal) — used only when the batch number was not received because
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
  deriving anyclass (FromJSON)

-- | Hand-written for wire order; see 'CbxInquiryReq'.
instance ToJSON CbxBatchNumReq where
  toJSON = A.object . batchNumPairs
  toEncoding = A.pairs . mconcat . batchNumPairs

batchNumPairs :: (A.KeyValue kv) => CbxBatchNumReq -> [kv]
batchNumPairs r =
  [ "gcif" A..= r.gcif,
    "iduser" A..= r.iduser,
    "reqdexctndt" A..= r.reqdexctndt,
    "filerefno" A..= r.filerefno
  ]

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
