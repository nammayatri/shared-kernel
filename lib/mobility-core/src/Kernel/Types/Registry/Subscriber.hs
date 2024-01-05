{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Registry.Subscriber where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time (UTCTime)
import EulerHS.Prelude
import Kernel.Types.Base64
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Beckn.Country (Country)
import Kernel.Types.Beckn.Domain (Domain)
import Kernel.Utils.Dhall (FromDhall)
import Servant.Client (BaseUrl)

data Subscriber = Subscriber
  { unique_key_id :: Text,
    subscriber_id :: Text,
    subscriber_url :: BaseUrl,
    _type :: SubscriberType,
    domain :: Domain,
    city :: [City],
    country :: Maybe Country,
    signing_public_key :: Base64,
    encr_public_key :: Maybe Base64,
    valid_from :: Maybe UTCTime,
    valid_until :: Maybe UTCTime,
    status :: Maybe SubscriberStatus,
    created :: UTCTime,
    updated :: UTCTime
  }
  deriving (Show, Generic)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "unique_key_id" -> "ukId"
        "_type" -> "type"
        other -> other
    }

instance FromJSON Subscriber where
  parseJSON (Object obj) = do
    unique_key_id <- obj .: "ukId"
    _type <- obj .: "type"
    subscriber_id <- obj .: "subscriber_id"
    subscriber_url <- obj .: "subscriber_url"
    domain <- obj .: "domain"
    country <- obj .: "country"
    signing_public_key <- obj .: "signing_public_key"
    encr_public_key <- obj .: "encr_public_key"
    valid_from <- obj .: "valid_from"
    valid_until <- obj .: "valid_until"
    status <- obj .: "status"
    updated <- obj .: "updated"
    created <- obj .: "created"
    city' <- obj .: "city"
    city <- parseCities city'
    pure Subscriber {..}
    where
      parseCities cities' = do
        let cities :: Result [City] = sequence . filter isSuccess $ map (fromJSON . String . T.strip) (T.splitOn "," cities')
        case cities of
          Success cities'' -> pure cities''
          Error _ -> error "failed"
      isSuccess a = case a of
        Success _ -> True
        Error _ -> False
  parseJSON wrongVal = typeMismatch "Object Subscriber" wrongVal

instance ToJSON Subscriber where
  toJSON Subscriber {..} = do
    object
      [ "ukId" .= unique_key_id,
        "type" .= _type,
        "subscriber_id" .= subscriber_id,
        "subscriber_url" .= subscriber_url,
        "domain" .= domain,
        "country" .= country,
        "signing_public_key" .= signing_public_key,
        "encr_public_key" .= encr_public_key,
        "valid_from" .= valid_from,
        "valid_until" .= valid_until,
        "status" .= status,
        "updated" .= updated,
        "created" .= created,
        "city" .= (parseCity city)
      ]
    where
      parseCity cities = String . (T.intercalate ",") $ map (toStringMe . toJSON) cities
        where
          toStringMe (String a) = a
          toStringMe e = error "Unexpected value type, expected String for City " <> show e

--   "incoming_affected" .= if null incomingAffectedSids then Nothing else Just incomingAffectedSids,
--   "outgoing_affected" .= if null outgoingAffectedSids then Nothing else Just outgoingAffectedSids,
--   "data" .= object (mkDataListItem <$> affectedPhones)
-- ]

--     let incomingAffectedSids = incomingAffected <&> (.phoneNumberSid)
--     let outgoingAffectedSids = outgoingAffected <&> (.phoneNumberSid)
--     let affectedPhones = incomingAffected <> filter (\phone -> phone.phoneNumberSid `notElem` incomingAffectedSids) outgoingAffected
--     object
--       [ "timestamp" .= timestamp,
--         "status_type" .= statusType,
--         "incoming_affected" .= if null incomingAffectedSids then Nothing else Just incomingAffectedSids,
--         "outgoing_affected" .= if null outgoingAffectedSids then Nothing else Just outgoingAffectedSids,
--         "data" .= object (mkDataListItem <$> affectedPhones)
--       ]
--     where
--       mkDataListItem PhoneNumber {..} = AesonKey.fromText phoneNumberSid .= object ["phone_number" .= phoneNumber]
-- instance FromJSON Subscriber where
--   parseJSON = genericParseJSON jsonOptions

-- instance ToJSON Subscriber where
--   toJSON = genericToJSON jsonOptions

data SubscriberType
  = BAP
  | BPP
  | BG
  | LREG
  | CREG
  | RREG
  deriving (Show, Read, Generic, Eq, ToSchema, FromJSON, ToJSON, FromDhall)

data SubscriberStatus
  = INITIATED
  | UNDER_SUBSCRIPTION
  | SUBSCRIBED
  | EXPIRED
  | UNSUBSCRIBED
  | INVALID_SSL
  deriving (Show, Read, Generic, FromJSON, ToJSON)

-- instance FromJSON ListCity where
--   parseJSON (String citiesStr) = do
--     let cities :: Result [City] = sequence . filter isSuccess $ map (fromJSON . String . T.strip) (T.splitOn "," citiesStr)
--     case cities of
--       Success cities' -> pure $ ListCity cities'
--       Error _ -> error "failed"
--     where
--       isSuccess a = case a of
--         Success _ -> True
--         Error _ -> False
--   parseJSON e = typeMismatch "String" e

-- instance ToJSON ListCity where
--   toJSON (ListCity cities) = String . (T.intercalate ",") $ map (toStringMe . toJSON) cities
--     where
--       toStringMe (String a) = a
--       toStringMe e = error "Unexpected value type, expected String for City " <> show e

-- data ExotelHeartbeatReq = ExotelHeartbeatReq
--   { timestamp :: UTCTime,
--     statusType :: StatusType,
--     incomingAffected :: [PhoneNumber],
--     outgoingAffected :: [PhoneNumber]
--   }
--   deriving stock (Show, Generic)

-- -- ToSchema instance is wrong, but anyway this api shouldn't be used by dashboard ops
-- instance ToSchema ExotelHeartbeatReq where
--   declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsWithSnakeCase

-- data StatusType = OK | WARNING | CRITICAL | PAYLOAD_TOO_LARGE
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data PhoneNumber = PhoneNumber
--   { phoneNumberSid :: Text,
--     phoneNumber :: Text
--   }
--   deriving stock (Show, Generic)

-- instance ToSchema PhoneNumber where
--   declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsWithSnakeCase

-- instance FromJSON ExotelHeartbeatReq where
--   parseJSON (Object obj) = do
--     timestamp <- obj .: "timestamp"
--     statusType <- obj .: "status_type"
--     incomingAffectedSids <- fromMaybe [] <$> (obj .:? "incoming_affected")
--     outgoingAffectedSids <- fromMaybe [] <$> (obj .:? "outgoing_affected")
--     dataObj <- obj .: "data"
--     incomingAffected <- for incomingAffectedSids $ \phoneNumberSid -> do
--       phoneNumber <- (dataObj .: AesonKey.fromText phoneNumberSid) >>= (.: "phone_number")
--       pure PhoneNumber {..}
--     outgoingAffected <- for outgoingAffectedSids $ \phoneNumberSid -> do
--       phoneNumber <- (dataObj .: AesonKey.fromText phoneNumberSid) >>= (.: "phone_number")
--       pure PhoneNumber {..}
--     pure ExotelHeartbeatReq {..}
--   parseJSON wrongVal = typeMismatch "Object ExotelHeartbeatReq" wrongVal

-- instance ToJSON ExotelHeartbeatReq where
--   toJSON ExotelHeartbeatReq {..} = do
--     let incomingAffectedSids = incomingAffected <&> (.phoneNumberSid)
--     let outgoingAffectedSids = outgoingAffected <&> (.phoneNumberSid)
--     let affectedPhones = incomingAffected <> filter (\phone -> phone.phoneNumberSid `notElem` incomingAffectedSids) outgoingAffected
--     object
--       [ "timestamp" .= timestamp,
--         "status_type" .= statusType,
--         "incoming_affected" .= if null incomingAffectedSids then Nothing else Just incomingAffectedSids,
--         "outgoing_affected" .= if null outgoingAffectedSids then Nothing else Just outgoingAffectedSids,
--         "data" .= object (mkDataListItem <$> affectedPhones)
--       ]
--     where
--       mkDataListItem PhoneNumber {..} = AesonKey.fromText phoneNumberSid .= object ["phone_number" .= phoneNumber]
