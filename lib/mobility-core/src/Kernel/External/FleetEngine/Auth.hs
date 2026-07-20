{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.FleetEngine.Auth
  ( TokenScope (..),
    fleetEngineAudience,
    parseServiceAccount,
    mintFleetEngineToken,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import qualified Kernel.Utils.JWT as JWT

-- | Audience for self-signed Fleet Engine JWTs (server, driver and consumer).
fleetEngineAudience :: Text
fleetEngineAudience = "https://fleetengine.googleapis.com/"

data TokenScope
  = ConsumerToken Text -- tripId
  | DriverToken Text -- vehicleId
  | ServerToken

-- | Decode the (decrypted) service-account JSON text.
parseServiceAccount :: Text -> Either String JWT.ServiceAccount
parseServiceAccount = A.eitherDecodeStrict . TE.encodeUtf8

-- | Mint a short-lived self-signed Fleet Engine JWT for the given scope.
mintFleetEngineToken :: JWT.ServiceAccount -> TokenScope -> Integer -> IO (Either String Text)
mintFleetEngineToken sa scope ttlSeconds =
  JWT.createSignedJWTWithClaims sa fleetEngineAudience ttlSeconds (authClaims scope)
  where
    authClaims :: TokenScope -> [(Text, A.Value)]
    authClaims (ConsumerToken tripId) = [("authorization", A.object ["tripid" A..= tripId])]
    authClaims (DriverToken vehicleId) = [("authorization", A.object ["vehicleid" A..= vehicleId])]
    -- Fleet Engine rejects tokens without an "authorization" claim
    -- ("JWT does not contain any scopes."); wildcard is the documented shape.
    authClaims ServerToken =
      [("authorization", A.object ["vehicleid" A..= ("*" :: Text), "tripid" A..= ("*" :: Text)])]
