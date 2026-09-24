{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Beckn.Ack where

import qualified Control.Lens as L
import Data.Aeson
import Data.Aeson.Types (unexpected)
import qualified Data.HashMap.Strict.InsOrd as HMSIO
import Data.OpenApi
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Error as BError

data AckResponse
  = Ack
  | Nack BError.Error
  deriving (Generic, Show)

instance ToSchema AckResponse where
  declareNamedSchema _ = do
    return $
      NamedSchema (Just "AckResponse") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.fromList [("message", messageSchema), ("error", errorSchema)]
          & required L..~ ["message"]
    where
      statusSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiString
          & enum_ L.?~ ["ACK", "NACK"]
          & Inline
      ackSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.singleton "status" statusSchema
          & required L..~ ["status"]
          & Inline
      messageSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.singleton "ack" ackSchema
          & required L..~ ["ack"]
          & Inline
      codeSchema = (mempty :: Schema) & type_ L.?~ OpenApiString & Inline
      messageFieldSchema = (mempty :: Schema) & type_ L.?~ OpenApiString & Inline
      errorSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.fromList [("code", codeSchema), ("message", messageFieldSchema)]
          & required L..~ ["code"]
          & Inline

-- ONDC TRV10 v2.1.0 envelope: `{message: {ack: {status}}, error?: {code, message}}`.
-- Error carries only `code` and `message` — the internal `type` and `path` fields are
-- not emitted (spec-optional, and we keep those in the in-memory Error record for logs).
--
-- Parser accepts the unwrapped shape (spec). A WAI-level wrapper middleware in the BPP
-- optionally rewraps outgoing bodies in `{response: ...}` for deployments whose BAPs
-- expect that shape; the response-wrapper middleware is the only place that knows about
-- the wrapper, keeping the wire-format contract explicit.
instance FromJSON AckResponse where
  parseJSON = withObject "AckResponse" $ \v -> do
    -- Accept both unwrapped (spec) and wrapped ({response: {...}}) bodies — the wrapper
    -- is handled by middleware at emit time, but a peer may echo our wrapped output back.
    inner <- (v .: "response") <|> pure v
    status <-
      (inner .: "message")
        >>= (.: "ack")
        >>= (.: "status")
    case status of
      String "ACK" -> pure Ack
      String "NACK" -> do
        errObj <- inner .: "error"
        errCode <- errObj .: "code"
        errMsg <- errObj .:? "message"
        pure $
          Nack
            BError.Error
              { BError._type = BError.INTERNAL_ERROR,
                BError.code = errCode,
                BError.path = Nothing,
                BError.message = errMsg
              }
      other -> unexpected other

instance ToJSON AckResponse where
  toJSON Ack =
    object
      [ "message" .= object ["ack" .= object ["status" .= ("ACK" :: Text)]]
      ]
  toJSON (Nack err) =
    object
      [ "message" .= object ["ack" .= object ["status" .= ("NACK" :: Text)]],
        "error"
          .= object
            [ "code" .= BError.code err,
              "message" .= BError.message err
            ]
      ]
