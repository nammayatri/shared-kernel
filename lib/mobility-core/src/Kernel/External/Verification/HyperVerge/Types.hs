module Kernel.External.Verification.HyperVerge.Types where

import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude hiding (error)
import Servant.Multipart.API (FileData (..), MultipartData (..), Tmp, ToMultipart (..))

data HyperVergeConfig = HyperVergeConfig
  { url :: BaseUrl,
    appId :: Text,
    appKey :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data HyperVergeSelfieValidationReq = HyperVergeSelfieValidationReq
  { image :: FilePath
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

instance ToMultipart Tmp HyperVergeSelfieValidationReq where
  toMultipart HyperVergeSelfieValidationReq {..} =
    MultipartData
      []
      [FileData "image" (T.pack image) "" image]

data FaceDetails = FaceDetails
  { liveFace :: ResultElement,
    qualityChecks :: Maybe QualityChecks
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SummaryDetails = SummaryDetails
  { code :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data Summary = Summary
  { action :: Text,
    details :: [SummaryDetails]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data QualityChecks = QualityChecks
  { eyesClosed :: Maybe ResultElement,
    maskPresent :: Maybe ResultElement,
    multipleFaces :: Maybe ResultElement,
    blur :: Maybe ResultElement,
    hat :: Maybe ResultElement,
    sunglasses :: Maybe ResultElement,
    readingGlasses :: Maybe ResultElement,
    bright :: Maybe ResultElement,
    dull :: Maybe ResultElement,
    headTurned :: Maybe ResultElement,
    lowQuality :: Maybe ResultElement
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data ResultElement = ResultElement
  { confidence :: Text,
    value :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data ValidationResult = ValidationResult
  { error :: Maybe Text,
    details :: Maybe FaceDetails,
    summary :: Maybe Summary
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data MetaData = MetaData
  { requestId :: Text,
    transactionId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data HyperVergeSelfieValidationRes = HyperVergeSelfieValidationRes
  { status :: Text,
    statusCode :: Int,
    result :: Maybe ValidationResult,
    metadata :: Maybe MetaData,
    error :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
