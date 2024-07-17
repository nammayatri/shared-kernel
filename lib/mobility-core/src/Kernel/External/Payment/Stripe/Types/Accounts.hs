{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Kernel.External.Payment.Stripe.Types.Accounts where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Kernel.External.Payment.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.JSON
import Servant
import Web.FormUrlEncoded

data AccountType
  = Standard
  | Express
  | Custom
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance ToHttpApiData AccountType where
  toQueryParam :: AccountType -> Text
  toQueryParam = T.pack . recursiveStrip . camelToSnake . show

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

instance ToHttpApiData AccountLossesPayer where
  toQueryParam :: AccountLossesPayer -> Text
  toQueryParam AccountLossesPayerStripe = "stripe"
  toQueryParam AccountLossesPayerApplication = "application"

instance FromJSON AccountLossesPayer where
  parseJSON = genericParseJSON accountLossesPayerJsonOptions

instance ToJSON AccountLossesPayer where
  toJSON = genericToJSON accountLossesPayerJsonOptions

newtype AccountLosses = AccountLosses
  { payments :: AccountLossesPayer
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToForm)

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

instance ToHttpApiData AccountFeePayer where
  toQueryParam :: AccountFeePayer -> Text
  toQueryParam AccountFeePayerAccount = "account"
  toQueryParam AccountFeePayerApplication = "application"

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

instance ToHttpApiData AccountRquirementCollector where
  toQueryParam :: AccountRquirementCollector -> Text
  toQueryParam AccountRquirementCollectorStripe = "stripe"
  toQueryParam AccountRquirementCollectorApplication = "application"

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

instance ToHttpApiData AccountDashboardType where
  toQueryParam :: AccountDashboardType -> Text
  toQueryParam AccountDashboardExpress = "express"
  toQueryParam AccountDashboardFull = "full"
  toQueryParam AccountDashboardNone = "none"

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

instance ToHttpApiData BusinessType where
  toQueryParam :: BusinessType -> Text
  toQueryParam = T.pack . recursiveStrip . camelToSnake . show

instance FromJSON BusinessType where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON BusinessType where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data AccountCapabilities = AccountCapabilities
  { card_payments :: CardPayments,
    -- cashapp_payments :: CashAppPayments,
    transfers :: Transfers
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype CardPayments = CardPayments
  { requested :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype Transfers = Transfers
  { requested :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype CashAppPayments = CashAppPayments
  { requested :: Bool
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype AccountSettings = AccountSettings
  { payouts :: PayoutsSettings
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PayoutsSettings = PayoutsSettings
  { debit_negative_balances :: Bool,
    statement_descriptor :: Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

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
  { _type :: Maybe AccountType,
    country :: Text, -- default to US, will fix later
    email :: Maybe Text,
    controller :: Maybe AccountController,
    capabilities :: Maybe AccountCapabilities,
    business_type :: BusinessType,
    settings :: Maybe AccountSettings,
    business_profile :: Maybe BusinessProfile, -- not for individual account
    individual :: Maybe IndividualDetails
    -- tos_acceptance :: Maybe TosAcceptance, -- can be revisit later
    -- metadata :: Maybe Metadata, -- can be used to store additional information
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance ToForm AccountsReq where
  toForm AccountsReq {..} =
    Form $
      foldl' insertOrAppend HM.empty $
        [ ("country", toQueryParam country),
          ("business_type", toQueryParam business_type)
        ]
          ++ catMaybes
            [ ("type",) <$> toQueryParam <$> _type,
              ("email",) <$> toQueryParam <$> email,
              ("capabilities[card_payments][requested]",) <$> (toQueryParam . (.card_payments.requested)) <$> capabilities,
              -- ("capabilities[cashapp_payments][requested]",) <$> (toQueryParam . (.cashapp_payments.requested)) <$> capabilities,
              ("capabilities[transfers][requested]",) <$> (toQueryParam . (.transfers.requested)) <$> capabilities,
              ("controller[fees][payer]",) <$> (toQueryParam . (.payer)) <$> ((.fees) =<< controller),
              ("controller[losses][payments]",) <$> (toQueryParam . (.payments)) <$> ((.losses) =<< controller),
              ("controller[requirement_collection]",) <$> toQueryParam <$> ((.requirement_collection) =<< controller),
              ("controller[stripe_dashboard][type]",) <$> (toQueryParam . (._type)) <$> ((.stripe_dashboard) =<< controller),
              ("settings[payouts][debit_negative_balances]",) <$> (toQueryParam . (.payouts.debit_negative_balances)) <$> settings,
              ("settings[payouts][statement_descriptor]",) <$> (toQueryParam . (.payouts.statement_descriptor)) <$> settings
            ]
          ++ maybe [] individualToForm individual
          ++ maybe [] businessProfileToForm business_profile

insertOrAppend :: HM.HashMap Text [Text] -> (Text, Text) -> HM.HashMap Text [Text]
insertOrAppend hm (k, v) = HM.insertWith (++) k [v] hm

individualToForm :: IndividualDetails -> [(Text, Text)]
individualToForm IndividualDetails {..} =
  [ ("individual[first_name]", toQueryParam first_name),
    ("individual[phone]", toQueryParam phone)
  ]
    ++ catMaybes
      [ ("individual[last_name]",) <$> toQueryParam <$> last_name,
        ("individual[email]",) <$> toQueryParam <$> email,
        ("individual[id_number]",) <$> toQueryParam <$> id_number,
        ("individual[ssn_last_4]",) <$> toQueryParam <$> ssn_last_4
      ]
    ++ maybe [] dobToForm dob
    ++ maybe [] addressToForm address

dobToForm :: DateOfBirth -> [(Text, Text)]
dobToForm DateOfBirth {..} =
  [ ("individual[dob][day]", toQueryParam day),
    ("individual[dob][month]", toQueryParam month),
    ("individual[dob][year]", toQueryParam year)
  ]

addressToForm :: Address -> [(Text, Text)]
addressToForm Address {..} =
  catMaybes
    [ ("individual[address][city]",) <$> toQueryParam <$> city,
      ("individual[address][country]",) <$> toQueryParam <$> country,
      ("individual[address][line1]",) <$> toQueryParam <$> line1,
      ("individual[address][line2]",) <$> toQueryParam <$> line2,
      ("individual[address][postal_code]",) <$> toQueryParam <$> postal_code,
      ("individual[address][state]",) <$> toQueryParam <$> state
    ]

businessProfileToForm :: BusinessProfile -> [(Text, Text)]
businessProfileToForm BusinessProfile {..} =
  catMaybes
    [ ("business_profile[mcc]",) <$> toQueryParam <$> mcc,
      ("business_profile[product_description]",) <$> toQueryParam <$> product_description,
      ("business_profile[support_phone]",) <$> toQueryParam <$> support_phone,
      ("business_profile[url]",) <$> toQueryParam <$> url
    ]
    ++ maybe [] businessSupportAdressToForm support_address

businessSupportAdressToForm :: BusinessSupportAddress -> [(Text, Text)]
businessSupportAdressToForm BusinessSupportAddress {..} =
  catMaybes
    [ ("business_profile[support_address][city]",) <$> toQueryParam <$> city,
      ("business_profile[support_address][country]",) <$> toQueryParam <$> country,
      ("business_profile[support_address][line1]",) <$> toQueryParam <$> line1,
      ("business_profile[support_address][line2]",) <$> toQueryParam <$> line2,
      ("business_profile[support_address][postal_code]",) <$> toQueryParam <$> postal_code,
      ("business_profile[support_address][state]",) <$> toQueryParam <$> state
    ]

instance FromJSON AccountsReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AccountsReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data BusinessProfile = BusinessProfile
  { mcc :: Maybe Text,
    product_description :: Maybe Text,
    support_phone :: Maybe Text,
    url :: Maybe Text,
    support_address :: Maybe BusinessSupportAddress
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data BusinessSupportAddress = BusinessSupportAddress
  { city :: Maybe Text,
    country :: Maybe Text,
    line1 :: Maybe Text,
    line2 :: Maybe Text,
    postal_code :: Maybe Text,
    state :: Maybe Text
  }
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data AccountResp = AccountResp
  { id :: AccountId,
    _object :: Text,
    charges_enabled :: Bool,
    details_submitted :: Bool
    -- Other paramters can be explored on basis of requirement.
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

instance ToHttpApiData AccountLinkType where
  toQueryParam :: AccountLinkType -> Text
  toQueryParam = T.pack . recursiveStrip . camelToSnake . show

instance FromJSON AccountLinkType where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON AccountLinkType where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data CollectionOptionsFields = CurrentlyDue | EventuallyDue
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance ToHttpApiData CollectionOptionsFields where
  toQueryParam :: CollectionOptionsFields -> Text
  toQueryParam = T.pack . recursiveStrip . camelToSnake . show

instance FromJSON CollectionOptionsFields where
  parseJSON = genericParseJSON constructorsWithCapitalToSnakeCase

instance ToJSON CollectionOptionsFields where
  toJSON = genericToJSON constructorsWithCapitalToSnakeCase

data CollectionOptionsFutureRequirements = Include | Omit
  deriving stock (Show, Eq, Generic, Read)
  deriving anyclass (ToSchema)

instance ToHttpApiData CollectionOptionsFutureRequirements where
  toQueryParam :: CollectionOptionsFutureRequirements -> Text
  toQueryParam = T.pack . recursiveStrip . camelToSnake . show

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

instance ToForm AccountLinkReq where
  toForm AccountLinkReq {..} =
    Form $
      foldl' insertOrAppend HM.empty $
        [ ("account", toQueryParam account),
          ("refresh_url", toQueryParam refresh_url),
          ("return_url", toQueryParam return_url),
          ("type", toQueryParam _type)
        ]
          ++ catMaybes
            [ ("collection_options[fields]",) <$> (toQueryParam . (.fields)) <$> collection_options,
              ("collection_options[future_requirements]",) <$> (toQueryParam . (.future_requirements)) <$> collection_options
            ]

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
