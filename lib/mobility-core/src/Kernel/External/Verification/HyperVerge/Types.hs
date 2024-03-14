{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Verification.HyperVerge.Types where

import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Servant.Multipart.Client

data HyperVergeConfig = HyperVergeConfig
  { hyperVergeFaceValidationUrl :: BaseUrl,
    hyperVergeAppId :: Text,
    hyperVergeAppKey :: Text,
    hyperVergeUidType :: Text,
    hyperVergeIgnoreSelfieQuality :: Text,
    matchThreshold :: Int
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

type AppKey = Text

type AppId = Text

data FaceValidationReq = FaceValidationReq
  { transactionId :: Text,
    uidType :: Text,
    uid :: Text,
    selfie :: FilePath,
    ignoreSelfieQuality :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

instance ToMultipart Tmp FaceValidationReq where
  toMultipart req =
    MultipartData
      [ Input "transactionId" (req.transactionId),
        Input "uidType" (req.uidType),
        Input "uid" (req.uid),
        Input "ignoreSelfieQuality" (req.ignoreSelfieQuality)
      ]
      [FileData (T.pack "selfie") (T.pack "") (T.pack "image/png") (req.selfie)]

data MetaData = MetaData
  { requestId :: Text,
    transactionId :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Data = Data
  { matchScore :: Int,
    match :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Summary = Summary
  { action :: Text,
    details :: [Details]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Details = Details
  { code :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Result = Result
  { resultData :: Data,
    summary :: Summary
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data FaceValidationRespSuccess = FaceValidationRespSuccess
  { status :: Text,
    statusCode :: Int,
    metaData :: MetaData,
    result :: Result
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data FaceValidationRespFailure = FaceValidationRespFailure
  { err :: Text,
    statusCode :: Int,
    status :: Text,
    requestId :: Text,
    transactionId :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
