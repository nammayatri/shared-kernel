{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payment.Stripe.Types.Accounts where

import Kernel.Prelude
import Kernel.Utils.JSON
import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime)

type AccountId = Text

data AccountType
  = Standard
  | Express
  | Custom
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountType where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON AccountType where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

-- A value indicating who is liable when this account can’t pay back negative balances resulting from payments. Defaults to Stripe
data AccountLossesPayer = AccountLossesPayerStripe | AccountLossesPayerApplication
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

accountLossesPayerJsonOptions :: Options
accountLossesPayerJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AccountLossesPayerStripe" -> "stripe"
        "AccountLossesPayerApplication" -> "application"
        x -> x
    }

instance FromJSON AccountLossesPayer where
  parseJSON = genericParseJSON accountLossesPayerJsonOptions

instance ToJSON AccountLossesPayer where
  toJSON = genericToJSON accountLossesPayerJsonOptions

newtype AccountLosses = AccountLosses
  { payments :: AccountLossesPayer
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


-- A value indicating the responsible payer of Stripe fees on this account. Defaults to Account
data AccountFeePayer = AccountFeePayerAccount | AccountFeePayerApplication
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

accountFeePayerJsonOptions :: Options
accountFeePayerJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AccountFeePayerAccount" -> "account"
        "AccountFeePayerApplication" -> "application"
        x -> x
    }

instance FromJSON AccountFeePayer where
  parseJSON = genericParseJSON accountFeePayerJsonOptions

instance ToJSON AccountFeePayer where
  toJSON = genericToJSON accountFeePayerJsonOptions

newtype AccountFees = AccountFees
  { payer :: AccountFeePayer
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- A value indicating responsibility for collecting updated information when requirements on the account are due or change. Defaults to Stripe.
data AccountRquirementCollector = AccountRquirementCollectorStripe | AccountRquirementCollectorApplication
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

accountRquirementCollectorJsonOptions :: Options
accountRquirementCollectorJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AccountRquirementCollectorStripe" -> "stripe"
        "AccountRquirementCollectorApplication" -> "application"
        x -> x
    }

instance FromJSON AccountRquirementCollector where
  parseJSON = genericParseJSON accountRquirementCollectorJsonOptions

instance ToJSON AccountRquirementCollector where
  toJSON = genericToJSON accountRquirementCollectorJsonOptions

-- A hash of configuration for Stripe-hosted dashboards
data AccountDashboardType = AccountDashboardExpress | AccountDashboardFull | AccountDashboardNone
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

accountDashboardTypeJsonOptions :: Options
accountDashboardTypeJsonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "AccountDashboardExpress" -> "express"
        "AccountDashboardFull" -> "full"
        "AccountDashboardNone" -> "none"
        x -> x
    }

instance FromJSON AccountDashboardType where
  parseJSON = genericParseJSON accountDashboardTypeJsonOptions

instance ToJSON AccountDashboardType where
  toJSON = genericToJSON accountDashboardTypeJsonOptions

newtype AccountDashboard = AccountDashboard
  { _type :: AccountDashboardType
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountDashboard where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AccountDashboard where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data AccountController = AccountController
  { fees :: Maybe AccountFees,
    losses :: Maybe AccountLosses,
    requirement_collection :: Maybe AccountRquirementCollector,
    stripe_dashboard :: Maybe AccountDashboard
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data BusinessType = Individual | Company | GovernmentEntity | NonProfit
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON BusinessType where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON BusinessType where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data DateOfBirth = DateOfBirth
  { day :: Int,
    month :: Int,
    year :: Int
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Address = Address
  { city :: Maybe Text,
    country :: Maybe Text,
    line1 :: Maybe Text,
    line2 :: Maybe Text,
    postal_code :: Maybe Text,
    state :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data IndividualDetails = IndividualDetails
  { first_name :: Text,
    last_name :: Maybe Text,
    dob :: Maybe DateOfBirth,
    address :: Maybe Address,
    email :: Maybe Text,
    id_number :: Maybe Text,
    phone :: Text,
    ssn_last_4 :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AccountsReq = AccountsReq
  { _type :: AccountType,
    country :: Text, -- default to US, will fix later
    email :: Maybe Text,
    controller :: Maybe AccountController,
    business_type :: BusinessType,
    -- business_profile :: Maybe BusinessProfile, -- not for individual account
    individual :: Maybe IndividualDetails
    -- tos_acceptance :: Maybe TosAcceptance, -- can be revisit later
    -- settings :: Maybe Settings, -- could come back to this later
    -- metadata :: Maybe Metadata, -- can be used to store additional information
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountsReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AccountsReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data AccountResp = AccountResp
  { id :: AccountId,
    _object :: Text,
    charges_enabled :: Bool,
    details_submitted :: Bool
    {-| Other paramters can be explored on basis of requirement. -}
    -- business_profile :: Maybe BusinessProfile,
    -- country :: Text,
    -- created :: Int,
    -- default_currency :: Text,
    -- email :: Maybe Text,
    -- external_accounts :: ExternalAccounts,
    -- individual :: IndividualDetails,
    -- metadata :: Metadata,
    -- payouts_enabled :: Bool,
    -- requirements :: Requirements,
    -- settings :: Settings,
    -- tos_acceptance :: TosAcceptance,
    -- type :: AccountType
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountResp where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AccountResp where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

----- ACCOUNT LINKS ----------------------------
data AccountLinkType = AccountOnboarding | AccountUpdate
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountLinkType where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON AccountLinkType where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data CollectionOptionsFields = CurrentlyDue | EventuallyDue
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON CollectionOptionsFields where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON CollectionOptionsFields where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data CollectionOptionsFutureRequirements = Include | Omit
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON CollectionOptionsFutureRequirements where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON CollectionOptionsFutureRequirements where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data CollectionOptions = CollectionOptions
  { fields :: CollectionOptionsFields,
    future_requirements :: CollectionOptionsFutureRequirements
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AccountLinkReq = AccountLinkReq
  { account :: Text,
    refresh_url :: Text,
    return_url :: Text,
    _type :: AccountLinkType,
    collection_options :: Maybe CollectionOptions
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountLinkReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AccountLinkReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data AccountLinkObject = AccountLinkObject
  { expires_at :: POSIXTime,
    url :: Text,
    _object :: Text,
    created :: POSIXTime
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance FromJSON AccountLinkObject where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AccountLinkObject where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
