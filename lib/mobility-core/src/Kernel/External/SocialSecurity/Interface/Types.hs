module Kernel.External.SocialSecurity.Interface.Types where

import Data.Time (Day)
import qualified Kernel.External.SocialSecurity.EShram.Types as EShram
import Kernel.Prelude

data SocialSecurityConfig
  = EShramSocialSecurityConfig EShram.EShramConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype AuthTokenResp = AuthTokenResp
  { authToken :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UnverifiedWorkersReq = UnverifiedWorkersReq
  { fromDate :: Day,
    toDate :: Day
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UnverifiedWorkersResp = UnverifiedWorkersResp
  { totalRecord :: Int,
    recordShared :: Int,
    remainingRecord :: Int,
    workers :: [UnverifiedWorker]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UnverifiedWorker = UnverifiedWorker
  { uanNo :: Text,
    panNo :: Text,
    yob :: Maybe Text,
    gender :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WorkerVerificationStatus = NotProcessed | Matched | NotMatched
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data WorkerVerification = WorkerVerification
  { uanNo :: Text,
    panNo :: Text,
    verificationStatus :: WorkerVerificationStatus,
    rejectionReason :: Maybe Text,
    dateOfOnboarding :: Maybe Day
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PushVerificationResp = PushVerificationResp
  { recordsUpdated :: RecordResult,
    recordsNotUpdated :: RecordResult,
    recordsFailed :: RecordResult,
    validationError :: RecordResult
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WorkerMonthlyUtilization = WorkerMonthlyUtilization
  { uanNo :: Text,
    month :: Int,
    year :: Int,
    noOfDays :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data MonthlyUtilizationResp = MonthlyUtilizationResp
  { recordsUpdated :: RecordResult,
    recordsInserted :: RecordResult,
    recordsNotFound :: RecordResult,
    validationError :: RecordResult
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RecordResult = RecordResult
  { recordCount :: Int,
    dataList :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
