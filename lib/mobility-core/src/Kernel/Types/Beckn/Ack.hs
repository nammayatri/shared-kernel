 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Beckn.Ack where

import Data.Aeson
import Data.Aeson.Types (unexpected)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data AckResponse = Ack
  deriving (Generic, Show, ToSchema)

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
      (.==) :: Text -> Value -> Value
      k .== v = Object (k .= v)
      infixr 9 .==
