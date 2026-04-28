{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kernel.External.Payout.Stripe.Types.ExternalAccount where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Kernel.External.Payout.Stripe.Types.Common
import Kernel.Prelude
import Kernel.Utils.JSON
import qualified Kernel.Utils.Schema as S
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

type AccountId = Text

newtype ExternalAccountId = ExternalAccountId Text
  deriving newtype (Show, Eq, ToHttpApiData, FromHttpApiData)

data ExternalAccountReq = ExternalAccountReq
  { _object :: Text, -- "bank_account" or "card"
    country :: Text,
    currency :: Text,
    account_number :: Maybe Text, -- For bank accounts: SENSITIVE
    routing_number :: Maybe Text, -- For bank accounts: SENSITIVE
    number :: Maybe Text, -- For cards: SENSITIVE
    exp_month :: Maybe Int, -- For cards: SENSITIVE
    exp_year :: Maybe Int, -- For cards: SENSITIVE
    cvc :: Maybe Text, -- For cards: SENSITIVE
    default_for_currency :: Maybe Bool,
    metadata :: Maybe Metadata
  }
  deriving stock (Generic)

instance ToForm ExternalAccountReq where
  toForm ExternalAccountReq {..} =
    Form $
      HM.fromList $
        [ ("external_account[object]", [toQueryParam _object]),
          ("external_account[country]", [toQueryParam country]),
          ("external_account[currency]", [toQueryParam currency])
        ]
          <> catMaybes
            [ ("external_account[account_number]",) . pure <$> account_number,
              ("external_account[routing_number]",) . pure <$> routing_number,
              ("external_account[number]",) . pure . toQueryParam <$> number,
              ("external_account[exp_month]",) . pure . toQueryParam <$> exp_month,
              ("external_account[exp_year]",) . pure . toQueryParam <$> exp_year,
              ("external_account[cvc]",) . pure <$> cvc,
              ("external_account[metadata][order_id]",) . pure . toQueryParam <$> (metadata >>= (.order_id)),
              ("external_account[metadata][customer_id]",) . pure . toQueryParam <$> (metadata >>= (.customer_id)),
              ("external_account[metadata][order_type]",) . pure . toQueryParam <$> (metadata >>= (.order_type))
            ]

data ExternalAccountObject = ExternalAccountObject
  { id :: Text,
    _object :: Text, -- "bank_account" or "card"
    account :: Text, -- The account this external account is attached to
    account_holder_name :: Maybe Text,
    account_holder_type :: Maybe Text, -- "individual" or "company"
    account_type :: Maybe Text, -- "checking", "savings", etc.
    available_payout_methods :: Maybe [Text],
    bank_name :: Maybe Text,
    country :: Text,
    currency :: Text,
    default_for_currency :: Maybe Bool,
    fingerprint :: Maybe Text,
    last4 :: Text,
    metadata :: Maybe Metadata,
    routing_number :: Maybe Text,
    status :: Text -- "new", "validated", "verified", "verification_failed", "errored"
  }
  deriving stock (Show, Generic)

instance FromJSON ExternalAccountObject where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ExternalAccountObject where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema ExternalAccountObject where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data ExternalAccountList = ExternalAccountList
  { _object :: Text, -- Always "list"
    _data :: [ExternalAccountObject],
    has_more :: Bool,
    url :: Text, -- API endpoint URL
    total_count :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromJSON ExternalAccountList where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ExternalAccountList where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema ExternalAccountList where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny

data UpdateExternalAccountReq = UpdateExternalAccountReq
  { default_for_currency :: Maybe Bool,
    metadata :: Maybe Metadata
  }
  deriving stock (Show, Generic)

instance ToForm UpdateExternalAccountReq where
  toForm UpdateExternalAccountReq {..} =
    Form $
      HM.fromList $
        catMaybes
          [ ("default_for_currency",) . pure . toQueryParam <$> default_for_currency,
            ("metadata[order_id]",) . pure . toQueryParam <$> (metadata >>= (.order_id)),
            ("metadata[customer_id]",) . pure . toQueryParam <$> (metadata >>= (.customer_id)),
            ("metadata[order_type]",) . pure . toQueryParam <$> (metadata >>= (.order_type))
          ]

data DeletedExternalAccount = DeletedExternalAccount
  { id :: Text,
    _object :: Text,
    deleted :: Bool
  }
  deriving stock (Show, Generic)

instance FromJSON DeletedExternalAccount where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DeletedExternalAccount where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema DeletedExternalAccount where
  declareNamedSchema = genericDeclareNamedSchema S.stripPrefixUnderscoreIfAny
