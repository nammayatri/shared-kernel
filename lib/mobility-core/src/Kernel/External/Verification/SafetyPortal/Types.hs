{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.SafetyPortal.Types where

import Data.Aeson
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (error, id, state)

newtype SearchAgentReq = SearchAgentReq
  { suspectReqList :: [Agent]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Agent = Agent
  { dl :: Maybe Text,
    voterId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data SearchAgentResp = SearchAgentResp
  { suspects :: [SearchAgent],
    summary :: Summary
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Summary = Summary
  { totalCount :: Int,
    count :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data SearchAgent = SearchAgent
  { createdAt :: UTCTime,
    dl :: Maybe Text,
    firstName :: Text,
    flagUpdatedAt :: UTCTime,
    flaggedBy :: Maybe [FlaggedBy],
    flaggedCounter :: Int,
    flaggedStatus :: FlaggedStatus,
    id :: Text,
    lastName :: Text,
    statusChangedReason :: Maybe Text,
    updatedAt :: UTCTime,
    voterId :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data FlaggedStatus = Flagged | Charged | Clean
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data FlaggedBy = FlaggedBy
  { flaggedCategory :: Text,
    partnerName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, Ord, Eq, Read)
