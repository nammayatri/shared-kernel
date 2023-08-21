{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Beckn.BecknAPIResponse where

import Data.Aeson
import Data.Aeson.Types ()
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data BecknAPIResponse = BecknAPIResponse
  { messageResponse :: Message,
    errorResponse :: Maybe BecknAPIResError
  }
  deriving (Generic, Show)

instance ToSchema BecknAPIResponse

instance FromJSON BecknAPIResponse where
  parseJSON = withObject "BecknAPIResponse" $ \v -> do
    msg <- v .: "message"
    err <- v .:? "error"
    return $ BecknAPIResponse msg err

instance ToJSON BecknAPIResponse where
  toJSON (BecknAPIResponse msg err) = object ["message" .= msg, "error" .= err]

data Ack = ACK | NACK deriving (Generic, Show)

instance ToSchema Ack

instance ToJSON Ack

instance FromJSON Ack

newtype Message = Message
  { ack :: Ack
  }
  deriving (Generic, Show)

instance ToSchema Message

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> do
    ackValue <- v .:? "ack"
    case ackValue of
      Just ("ACK" :: Text) -> return $ Message ACK
      Just ("NACK" :: Text) -> return $ Message NACK
      _ -> fail "Invalid ack value"

instance ToJSON Message where
  toJSON (Message ackValue) = object ["ack" .= ackValue]

data BecknAPIResError = BecknAPIResError
  { errorType :: BeckErrorType,
    code :: Text,
    message :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema BecknAPIResError

instance FromJSON BecknAPIResError where
  parseJSON = withObject "BecknAPIResError" $ \v -> do
    errorTypeValue <- v .: "errorType"
    codeValue <- v .: "code"
    messageValue <- v .:? "message"
    return $ BecknAPIResError errorTypeValue codeValue messageValue

instance ToJSON BecknAPIResError where
  toJSON (BecknAPIResError errorTypeValue codeValue messageValue) =
    object
      [ "errorType" .= errorTypeValue,
        "code" .= codeValue,
        "message" .= messageValue
      ]

data BeckErrorType
  = ContextError
  | CoreError
  | DomainError
  | PolicyError
  | JsonSchemaError
  deriving (Generic, Show)

instance ToSchema BeckErrorType

instance ToJSON BeckErrorType where
  toJSON ContextError = String "CONTEXT-ERROR"
  toJSON CoreError = String "CORE-ERROR"
  toJSON DomainError = String "DOMAIN-ERROR"
  toJSON PolicyError = String "POLICY-ERROR"
  toJSON JsonSchemaError = String "JSON-SCHEMA-ERROR"

instance FromJSON BeckErrorType where
  parseJSON (String "CONTEXT-ERROR") = return ContextError
  parseJSON (String "CORE-ERROR") = return CoreError
  parseJSON (String "DOMAIN-ERROR") = return DomainError
  parseJSON (String "POLICY-ERROR") = return PolicyError
  parseJSON (String "JSON-SCHEMA-ERROR") = return JsonSchemaError
  parseJSON invalid = fail $ "Invalid BeckErrorType: " ++ show invalid

getTtlExpiredRes :: Applicative m => m BecknAPIResponse
getTtlExpiredRes = do
  let errorResponse =
        Just
          BecknAPIResError
            { errorType = PolicyError,
              code = "CODEHERE",
              message = Just "TTL is Expired"
            }
  pure
    BecknAPIResponse
      { messageResponse =
          Message
            { ack = NACK
            },
        ..
      }

getSuccessRes :: BecknAPIResponse
getSuccessRes =
  BecknAPIResponse
    { messageResponse =
        Message
          { ack = ACK
          },
      errorResponse = Nothing
    }
