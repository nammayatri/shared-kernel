{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Kernel.Types.Beckn.Ack where

import qualified Control.Lens as L
import Data.Aeson
import qualified Data.Aeson.Key as AesonKey (Key)
import Data.Aeson.Types (unexpected)
import qualified Data.HashMap.Strict.InsOrd as HMSIO
import Data.OpenApi
import EulerHS.Prelude

data AckResponse = Ack
  deriving (Generic, Show)

instance ToSchema AckResponse where
  declareNamedSchema _ = do
    return $
      NamedSchema (Just "AckResponse") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.singleton "message" messageSchema
          & required L..~ ["message"]
    where
      statusSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiString
          & enum_ L.?~ ["ACK"]
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

instance FromJSON AckResponse where
  parseJSON = withObject "Ack" $ \v -> do
    status <-
      (v .: "message")
        >>= (.: "ack")
        >>= (.: "status")
    unless (status == String "ACK") (unexpected status)
    pure Ack

instance ToJSON AckResponse where
  toJSON Ack = "message" .== "ack" .== "status" .== String "ACK"
    where
      (.==) :: AesonKey.Key -> Value -> Value
      k .== v = Object (k .= v)
      infixr 9 .==
