{-# LANGUAGE DerivingVia #-}

module Kernel.Types.Beckn.Error where

import Kernel.Utils.GenericPretty (PrettyShow, Showable (Showable))
import Kernel.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Error = Error
  { _type :: ErrorType,
    code :: Text,
    path :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, PrettyShow)

data ErrorType
  = CONTEXT_ERROR
  | CORE_ERROR
  | INTERNAL_ERROR -- Not a spec value. TODO: get rid of it.
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)
  deriving (PrettyShow) via Showable ErrorType

instance FromJSON ErrorType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Error where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Error where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
