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

module Kernel.External.BackgroundVerification.Checkr.Types where

import Data.Aeson
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Beckn.City
import Kernel.Types.Beckn.Country
import Kernel.Types.Beckn.IndianState
import Kernel.Utils.JSON
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

castCityToCheckrCity :: City -> Text
castCityToCheckrCity Minneapolis = "Minneapolis"
castCityToCheckrCity _ = "Minneapolis"

castCountryToCheckrCountry :: Country -> Text
castCountryToCheckrCountry USA = "US"
castCountryToCheckrCountry _ = "US"

castStateToCheckrState :: IndianState -> Text
castStateToCheckrState Minnesota = "MN"
castStateToCheckrState _ = "MN"

data CreateCandidateReq = CreateCandidateReq
  { email :: Text,
    ssn :: Maybe Text,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    phone :: Maybe Text,
    dob :: Maybe UTCTime,
    workLocationCountry :: Country,
    workLocationState :: IndianState,
    workLocationCity :: City,
    driverLicenseNumber :: Maybe Text,
    driverLicenseState :: Maybe Text,
    zipCode :: Maybe Text
  }
  deriving (Generic, Eq, Show)

instance ToForm CreateCandidateReq where
  toForm CreateCandidateReq {..} =
    [ ("email", toQueryParam email),
      ("ssn", toQueryParam ssn),
      ("first_name", toQueryParam firstName),
      ("work_locations[][country]", toQueryParam $ castCountryToCheckrCountry workLocationCountry),
      ("work_locations[][state]", toQueryParam $ castStateToCheckrState workLocationState),
      ("work_locations[][city]", toQueryParam $ castCityToCheckrCity workLocationCity)
    ]
      <> maybe [] (\val -> [("middle_name", toQueryParam val)]) middleName
      <> maybe [] (\val -> [("last_name", toQueryParam val)]) lastName
      <> maybe [] (\val -> [("phone", toQueryParam val)]) phone
      <> maybe [] (\val -> [("dob", toQueryParam $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" val)]) dob
      <> maybe [] (\val -> [("driver_license_number", toQueryParam val)]) driverLicenseNumber
      <> maybe [] (\val -> [("driver_license_state", toQueryParam val)]) driverLicenseState
      <> maybe [] (\val -> [("zipcode", toQueryParam val)]) zipCode

newtype CreateCandidateResp = CreateCandidateResp
  { id :: Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON CreateCandidateResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON CreateCandidateResp where
  toJSON = genericToJSON constructorsWithSnakeCase

data CreateInvitationReq = CreateInvitationReq
  { candidateId :: Text,
    package :: Text,
    workLocationCountry :: Country,
    workLocationState :: IndianState,
    workLocationCity :: City
  }
  deriving (Generic, Eq, Show)

instance ToForm CreateInvitationReq where
  toForm CreateInvitationReq {..} =
    [ ("candidate_id", candidateId),
      ("package", package),
      ("work_locations[][country]", castCountryToCheckrCountry workLocationCountry),
      ("work_locations[][state]", castStateToCheckrState workLocationState),
      ("work_locations[][city]", castCityToCheckrCity workLocationCity)
    ]

data CreateInvitationResp = CreateInvitationResp
  { id :: Text,
    invitationUrl :: BaseUrl,
    expiresAt :: UTCTime
  }
  deriving (Generic, Eq, Show)

instance FromJSON CreateInvitationResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON CreateInvitationResp where
  toJSON = genericToJSON constructorsWithSnakeCase

data GetInvitationResp = GetInvitationResp
  { id :: Text,
    invitationUrl :: BaseUrl,
    status :: Text, -- "pending | completed"
    reportId :: Maybe Text,
    expiresAt :: UTCTime
  }
  deriving (Generic, Eq, Show)

instance FromJSON GetInvitationResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON GetInvitationResp where
  toJSON = genericToJSON constructorsWithSnakeCase

data GetReportResp = GetReportResp
  { id :: Text,
    adjudication :: Maybe Text, -- "engaged"
    status :: Text, -- "pending | completed"
    reportId :: Maybe Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON GetReportResp where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON GetReportResp where
  toJSON = genericToJSON constructorsWithSnakeCase
