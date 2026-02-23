{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Kernel.External.Plasma.LMS.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toUpper)
import Data.OpenApi hiding (description, title)
import qualified Data.Text as T
import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError hiding (Error)
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Utils.JSON (camelToSnakeCase)

-- | Module status enum
data ModuleStatus
  = COMPLETED
  | IN_PROGRESS
  | NOT_STARTED
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ModuleStatus where
  parseJSON (String "COMPLETED") = pure COMPLETED
  parseJSON (String "IN_PROGRESS") = pure IN_PROGRESS
  parseJSON (String "NOT_STARTED") = pure NOT_STARTED
  parseJSON (String s) = fail $ "Invalid ModuleStatus: " <> T.unpack s
  parseJSON e = typeMismatch "String" e

instance ToJSON ModuleStatus where
  toJSON COMPLETED = "COMPLETED"
  toJSON IN_PROGRESS = "IN_PROGRESS"
  toJSON NOT_STARTED = "NOT_STARTED"

-- | Convert snake_case to camelCase for parsing JSON
-- This converts "completed_items" -> "completedItems"
snakeToCamelCase :: String -> String
snakeToCamelCase = go . splitOn '_'
  where
    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn delimiter str =
      let (before, remainder) = break (== delimiter) str
          after = drop 1 remainder
       in before : if null after then [] else splitOn delimiter after
    go [] = []
    go (x : xs) = x ++ concatMap capitalize xs
    capitalize [] = []
    capitalize (c : cs) = toUpper c : cs

-- | LMS Module List Item
data LMSModuleListItem = LMSModuleListItem
  { category :: Text,
    completedItems :: Int,
    completionPercentage :: Double,
    description :: Text,
    duration :: Int,
    moduleId :: Text,
    status :: ModuleStatus,
    thumbnailUrl :: Maybe Text,
    title :: Text,
    totalItems :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON LMSModuleListItem where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = snakeToCamelCase
        }

instance ToJSON LMSModuleListItem where
  toJSON =
    genericToJSON $
      defaultOptions
        { fieldLabelModifier = camelToSnakeCase
        }

-- | Response type for LMS modules API
-- The API returns a list of LMSModuleListItem
type LMSModulesResp = [LMSModuleListItem]

-- | Error type for LMS API
newtype LMSError = LMSError {message :: Text}
  deriving (Show, Generic)

instance IsAPIError LMSError

deriving newtype instance ToJSON LMSError

deriving newtype instance FromJSON LMSError

deriving newtype instance ToSchema LMSError

instance FromResponse LMSError where
  fromResponse = fromJsonResponse

instance IsBaseError LMSError where
  toMessage _ = Just "LMS_ERROR"

instance IsHTTPError LMSError where
  toErrorCode _ = "CORE003"
  toHttpCode _ = E500

instance IsBecknAPIError LMSError where
  toType _ = DOMAIN_ERROR

instanceExceptionWithParent 'HTTPException ''LMSError
