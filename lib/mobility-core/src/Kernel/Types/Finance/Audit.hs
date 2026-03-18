{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

-- | Canonical audit types for the financial system.
--   These are the SINGLE SOURCE OF TRUTH for audit enums across all services.
--   finance-kernel imports these types directly via its YAML spec.
--   Do NOT define duplicate AuditAction/AuditEntityType/AuditActorType in
--   downstream services -- always import from this module.
--
-- TODO: Add contract tests for JSON round-trip, HTTP param round-trip, and Beam storage round-trip.
-- TODO: Add derivePersistField for these enums if downstream services use Persistent/Esqueleto code paths.
module Kernel.Types.Finance.Audit
  ( -- * Entity types that can be audited
    AuditEntityType (..),

    -- * Actions that can be performed on audited entities
    AuditAction (..),

    -- * Types of actors who perform audit-worthy actions
    AuditActorType (..),
  )
where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
-- TODO: HTTP param instances use JSON encoding (quoted strings) which may cause interoperability
-- issues with plain-text enum expectations. Consider switching to Show/Read-based instances for HTTP params.
import Kernel.Utils.TH (mkHttpInstancesForEnum)

-- | Types of entities tracked in the financial audit trail.
--   Add new entity types here when introducing new auditable financial objects.
data AuditEntityType
  = Account
  | LedgerEntry
  | Invoice
  | PaymentOrder
  | PayoutRequest
  | SubscriptionPurchase
  | PenaltyRecord
  | TollReimbursement
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''AuditEntityType)

$(mkHttpInstancesForEnum ''AuditEntityType)

-- | Actions that can be taken on financial entities.
--   This is the unified set used by both shared-kernel and finance-kernel.
--   The finance-kernel YAML spec imports this type directly.
data AuditAction
  = Created
  | Updated
  | Reversed
  | StatusChanged
  | Voided
  | Settled
  | Disputed
  | Resolved
  | Approved
  | Rejected
  | SubmittedForApproval
  | Escalated
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''AuditAction)

$(mkHttpInstancesForEnum ''AuditAction)

-- | Types of actors who perform auditable actions.
--   Used for attribution and compliance reporting.
data AuditActorType
  = System
  | AdminUser
  | Driver
  | Customer
  | Scheduler
  | Webhook
  | Maker
  | Checker
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''AuditActorType)

$(mkHttpInstancesForEnum ''AuditActorType)
