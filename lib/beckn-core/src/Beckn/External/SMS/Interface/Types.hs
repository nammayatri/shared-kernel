{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.External.SMS.Interface.Types
  ( module Beckn.External.SMS.Interface.Types,
  )
where

import qualified Beckn.External.SMS.ExotelSms.Config as ExotelSms
import qualified Beckn.External.SMS.MyValueFirst.Config as MyValueFirst
import qualified Beckn.External.SMS.Types as T
import Beckn.Prelude
import Beckn.Types.Servant
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import Servant

data SmsHandler m = SmsHandler
  { getProvidersPriorityList :: m [T.SmsService],
    getProviderConfig :: T.SmsService -> m SmsServiceConfig
  }

data SmsServiceConfig = MyValueFirstConfig MyValueFirst.MyValueFirstCfg | ExotelSmsConfig ExotelSms.ExotelSmsCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] SmsServiceConfig

data SendSMSReq = SendSMSReq
  { smsBody :: Text,
    phoneNumber :: Text,
    sender :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data SendSMSRes = Success | Fail | Pending | UnknownError
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

instance MimeUnrender PlainText_ISO_8859_1 SendSMSRes where
  mimeUnrender _ = Right . parseSendSMSRes . T.decodeLatin1 . toStrict

instance MimeRender PlainText_ISO_8859_1 SendSMSRes where
  mimeRender _ = fromStrict . T.encodeUtf8 . sendOtpResToText

parseSendSMSRes :: Text -> SendSMSRes
parseSendSMSRes txt
  | "Success" `T.isPrefixOf` txt = Success
  | "Fail" `T.isPrefixOf` txt = Fail
  | "Pending" `T.isPrefixOf` txt = Pending
  | otherwise = UnknownError

sendOtpResToText :: SendSMSRes -> Text
sendOtpResToText = \case
  Success -> "Success sms"
  Fail -> "Fail"
  Pending -> "Pending"
  UnknownError -> "unknown request"

type OtpTemplate = Text
