{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Kernel.External.Payout.Interface.Types
  ( module Kernel.External.Payout.Interface.Types,
    module Reexport,
  )
where

import Data.Time.Calendar (Day)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import qualified Kernel.External.Payout.HdfcCbx.Config as HdfcCbx
import qualified Kernel.External.Payout.Juspay.Config as Juspay
import Kernel.External.Payout.Juspay.Types as Reexport (Fulfillment (..), PayoutOrderStatus (..))
import qualified Kernel.External.Payout.Stripe.Config as Stripe
import Kernel.External.Payout.Stripe.Types as Reexport (TransferId (..))
import Kernel.External.Payout.Types as Reexport (BulkStatusCheckPlan (..), defaultBulkStatusCheckPlan, sanitizeBulkStatusCheckPlan)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common
import Servant.API (ToHttpApiData (..))

data PayoutServiceConfig
  = JuspayConfig Juspay.JuspayConfig
  | StripeConfig Stripe.StripeConfig
  | HdfcCbxConfig HdfcCbx.HdfcCbxConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderStatusPayoutResp
  = OrderStatusPayoutResp
      { payoutOrderId :: Text,
        idAssignedByServiceProvider :: Maybe Text, -- Stripe specific
        payoutStatus :: PayoutOrderStatus,
        orderType :: Maybe Text,
        merchantCustomerId :: Maybe Text,
        amount :: HighPrecMoney,
        createdAt :: Maybe Text,
        updatedAt :: Maybe Text
      }
  | BadStatusResp
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type AccountId = Text

data CreatePayoutOrderReq = CreatePayoutOrderReq
  { orderId :: Text,
    amount :: HighPrecMoney,
    transferAmount :: HighPrecMoney,
    currency :: Currency,
    customerPhone :: Text,
    customerEmail :: Text,
    customerId :: Text,
    orderType :: Text,
    remark :: Text,
    customerName :: Text,
    customerVpa :: Maybe Text, -- Juspay specific
    mRoutingId :: Maybe Text, -- Juspay specific
    mConnectedAccountId :: Maybe Stripe.AccountId, -- Stripe specific
    mExternalAccountId :: Maybe Text, -- Stripe specific, default will be used in case of Nothing
    mBankAccountNumber :: Maybe Text, -- bulk partners on bank rails
    mBankIfscCode :: Maybe Text, -- bulk partners on bank rails
    mBeneficiaryName :: Maybe Text -- bulk partners; HDFC caps this at 40 on NEFT and RTGS
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreatePayoutOrderResp = CreatePayoutOrderResp
  { orderId :: Text,
    status :: PayoutOrderStatus,
    transferStatus :: Maybe TransferStatus, -- Stripe specific
    orderType :: Maybe Text,
    transferId :: Maybe TransferId, -- Stripe specific
    idAssignedByServiceProvider :: Maybe Text, -- Stripe specific
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    udf3 :: Maybe Text,
    udf4 :: Maybe Text,
    udf5 :: Maybe Text,
    amount :: HighPrecMoney,
    refunds :: Maybe [Text],
    payments :: Maybe [Text],
    fulfillments :: Maybe [Fulfillment],
    customerId :: Maybe Text,
    merchantTopUpAmount :: Maybe HighPrecMoney -- Stripe specific: extra amount transferred to cover Fleet VA shortfall
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TransferStatus = TRANSFER_INITIATED | TRANSFERRED | TRANSFER_FAILED
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Expand = ExpandFulfillment | ExpandPayment | ExpandRefund
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema, Ord, Read)

$(mkBeamInstancesForEnum ''Expand)

derivePersistField "Expand"

instance ToHttpApiData Expand where
  toUrlPiece ExpandFulfillment = "fulfillment"
  toUrlPiece ExpandPayment = "payment"
  toUrlPiece ExpandRefund = "refund"

data PayoutOrderStatusReq = PayoutOrderStatusReq
  { orderId :: Text,
    idAssignedByServiceProvider :: Maybe Text, -- Stripe specific
    mbExpand :: Maybe Expand, -- Juspay specific
    mRoutingId :: Maybe Text, -- Juspay specific
    mConnectedAccountId :: Maybe AccountId, -- Stripe specific
    transferStatus :: Maybe TransferStatus,
    transferId :: Maybe TransferId
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type PayoutOrderStatusResp = CreatePayoutOrderResp

type CreateExternalPayoutReq = CreatePayoutOrderReq

type ExternalPayoutOrderStatusReq = PayoutOrderStatusReq

data CreateExternalPayoutResp = CreateExternalPayoutResp
  { orderId :: Text,
    status :: PayoutOrderStatus, -- Stripe specific: payout status from driver/fleet connected account to driver/fleet bank account/card
    orderType :: Maybe Text,
    idAssignedByServiceProvider :: Maybe Text, -- Stripe specific
    amount :: HighPrecMoney,
    customerId :: Maybe Text
  }

type ExternalPayoutOrderStatusResp = CreateExternalPayoutResp

data TransferAccount = TransferConnectedAccount AccountId | TransferPlatformAccount

data CreateTransferReq = CreateTransferReq
  { amount :: HighPrecMoney,
    currency :: Currency,
    senderAccountId :: TransferAccount,
    destinationAccount :: TransferAccount,
    description :: Maybe Text
  }

$(mkBeamInstancesForEnum ''TransferStatus)

data CreateTransferResp = CreateTransferResp
  { transferId :: TransferId,
    transferStatus :: TransferStatus
  }

--------------------------------------------------------------------------------
-- Bulk payouts
--
-- Partner-neutral by construction: a second bulk partner implements the same four
-- functions without any of these types changing. Nothing HDFC-shaped appears here --
-- the adapter is the only layer that knows their vocabulary.
--------------------------------------------------------------------------------

-- | Rail the money travels on. Bulk partners are account-based; there is no UPI rail.
data PayoutRail = RailA2A | RailNEFT | RailRTGS | RailIMPS
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''PayoutRail)

derivePersistField "PayoutRail"

-- | Which kind of reference the partner returned for a settled item. The same field is a
-- UTR on NEFT and RTGS, an FT number intra-bank and an RRN on IMPS, so it is typed rather
-- than named after one rail's vocabulary.
data SettlementRefType = UTR | FT_NUMBER | RRN | PARTNER_REF
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''SettlementRefType)

derivePersistField "SettlementRefType"

-- | What the bulk stages need to know about a payout partner, without knowing which partner it
-- is. Read once per city and carried through claiming, submission and status checks, so no
-- caller downstream ever matches on a partner's config constructor.
data BulkPartnerCaps = BulkPartnerCaps
  { -- | Recorded on the batch and in logs, so an operator can tell who a batch went to.
    partnerName :: Text,
    -- | The partner's own ceiling on items in one submission. Callers chunk to it before
    -- submitting; the adapter also enforces it, so an oversized batch is refused either way.
    maxItemsPerBatch :: Int,
    -- | How often to ask this partner about a submitted batch.
    statusCheckPlan :: BulkStatusCheckPlan
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BulkPayoutItem = BulkPayoutItem
  { -- | Our reference for this item. The partner echoes it on every inquiry row, so it is
    -- the only reliable way back to a payout order.
    itemRef :: Text,
    amount :: HighPrecMoney,
    currency :: Currency,
    bankAccountNumber :: Text,
    bankIfscCode :: Text,
    beneficiaryName :: Text,
    -- | Partner-side beneficiary code, where registration is required before payment.
    beneficiaryCode :: Maybe Text,
    beneficiaryEmail :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BulkPayoutReq = BulkPayoutReq
  { -- | Ours, generated before the call so a timed-out submission stays recoverable.
    clientRefNo :: Text,
    executionDate :: Day,
    rail :: PayoutRail,
    items :: [BulkPayoutItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Note there is no timeout constructor. A timeout is not an outcome: the caller sees the
-- exception and must hold its reservations rather than releasing them, because the partner
-- may well have accepted the batch.
data BulkPayoutResp
  = -- | Accepted for processing, with the reference the partner assigned. Depending on their
    -- configuration this may mean queued for a human approver rather than accepted for payment.
    BulkAccepted {partnerBatchRef :: Text}
  | -- | Accepted, but the partner quoted no reference for it. The submission landed, so nothing
    -- may be released or re-sent; the reference has to be recovered before it can be tracked.
    BulkAcceptedNoRef {acceptedReason :: Text}
  | -- | The partner refused the file itself, inside an ordinary response rather than an error.
    -- Nothing was accepted.
    BulkRejected {code :: Text, reason :: Text}
  | -- | The partner already holds a submission under this reference. It says nothing about whose
    -- file that is, or whether it should be paid, so it is never a reason to send the money again.
    BulkDuplicate {existingBatchRef :: Maybe Text, duplicateReason :: Text}
  | -- | The request was refused before the partner's banking layer saw it -- every non-200 arrives
    -- as the same problem document, whose code is the most specific thing it carries.
    BulkGatewayFailed {gatewayCode :: Text, gatewayReason :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BulkStatusCheckReq = BulkStatusCheckReq
  { partnerBatchRef :: Maybe Text,
    clientRefNo :: Text,
    executionDate :: Day
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Per-item outcome, keyed on 'itemRef'.
--
-- Partners generally repeat every row on every inquiry rather than sending a delta, so
-- applying these must be idempotent.
data BulkItemOutcome
  = -- | In flight. Keep the reservation and inquire again if budget remains.
    ItemInterim {note :: Maybe Text}
  | -- | In flight, but held in the partner's maker-checker queue awaiting a human approval step.
    -- Distinguished from 'ItemInterim' so a whole batch of these can be shown as awaiting approval
    -- rather than merely being polled.
    ItemPendingApproval {note :: Maybe Text}
  | ItemProcessed {settlementRef :: Text, refType :: SettlementRefType}
  | -- | Terminal refusal. Both halves are the partner's own: @code@ is whichever coded axis refused
    -- it -- @codstatus@ for a validation rejection, @rbistatus@ for a post-debit return -- and
    -- @detail@ is their text for it. No category of ours: which axis refused it is already readable
    -- from the settlement status (TRANSFER_FAILED for a return, absent for a validation rejection),
    -- and a coarser label of our own only ever disagreed with those two.
    ItemRejected {code :: Maybe Text, detail :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BulkStatusCheckResp
  = -- | Accepted but nothing to report yet.
    StatusCheckNotReady
  | -- | Per item: its ref, the canonical outcome, and the settlement (rbistatus) status to store
    -- as @transferStatus@ (a mirror of rbistatus; 'Nothing' when there is no RBI settlement).
    StatusCheckResolved [(Text, BulkItemOutcome, Maybe TransferStatus)]
  | -- | No data found. Verify the request parameters; do not re-check in a loop.
    StatusCheckNoData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Recovering a partner batch reference after a submission timed out. Every input is
-- something the caller wrote before the call, which is why it is recoverable at all.
data BatchRefRecoveryReq = BatchRefRecoveryReq
  { clientRefNo :: Text,
    executionDate :: Day
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BatchRefRecoveryResp
  = BatchRefFound {partnerBatchRef :: Text}
  | -- | The partner has no record of it, so the submission never landed.
    BatchRefNotFound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
