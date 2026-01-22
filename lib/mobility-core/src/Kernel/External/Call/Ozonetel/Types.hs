{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.Call.Ozonetel.Types where

import Data.Aeson.TH
import Kernel.Prelude

-- | Campaign Data in Response (defined first as it's used in OzonetelAddCampaignDataResp)
data OzonetelCampaignData = OzonetelCampaignData
  { campaignId :: Maybe Text,
    phoneNumber :: Maybe Text,
    name :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Ozonetel Add Campaign Data Request
data OzonetelAddCampaignDataReq = OzonetelAddCampaignDataReq
  { campaignName :: Text,
    phoneNumber :: Text,
    name :: Text,
    action :: Text,
    userName :: Text,
    checkDuplicate :: Bool
  }
  deriving (Show, Generic, Eq, ToJSON, FromJSON, ToSchema)

-- | Ozonetel Add Campaign Data Response
data OzonetelAddCampaignDataResp = OzonetelAddCampaignDataResp
  { status :: Maybe Text,
    message :: Maybe Text,
    data_ :: Maybe OzonetelCampaignData
  }
  deriving (Show, Generic, ToSchema)

-- Use default JSON options with custom field label modifier for "data_" -> "data"
$(deriveFromJSON defaultOptions {fieldLabelModifier = \s -> if s == "data_" then "data" else s} ''OzonetelAddCampaignDataResp)
$(deriveToJSON defaultOptions {fieldLabelModifier = \s -> if s == "data_" then "data" else s} ''OzonetelAddCampaignDataResp)
