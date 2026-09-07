{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payout.HdfcCbx.Config where

import Kernel.External.Encryption
import Kernel.Prelude

-- | Connection and identity for HDFC CBX bulk payouts.
--
-- Deliberately not a constructor on 'PayoutServiceConfig' yet: the adapter is written
-- against this concrete type so that adding the variant stays a single, coordinated change.
data HdfcCbxConfig = HdfcCbxConfig
  { -- | API host, e.g. https://api.hdfcuat.bank.in
    url :: BaseUrl,
    -- | OAuth token endpoint. Two HDFC documents disagree on host and version; confirm before use.
    tokenUrl :: BaseUrl,
    -- | OAuth client credentials, sent as HTTP Basic on the token call.
    consumerKey :: Text,
    consumerSecret :: EncryptedField 'AsEncrypted Text,
    -- | Registered on the API portal App; sent as a form parameter on the token call.
    scope :: Text,
    -- | Sent on every request as the @apikey@ header, separate from the bearer token.
    apiKey :: EncryptedField 'AsEncrypted Text,
    -- | Request identity, all from CBX setup rather than the API portal.
    clientCode :: Text,
    -- | Domain ID. Called @groupid@ on payment and inquiry, @gcif@ on batch-number inquiry.
    groupId :: Text,
    -- | Maker ID; must be explicitly enabled for API access.
    userId :: Text,
    -- | Our RSA private key, PEM encoded. Signs outbound JWS and decrypts inbound JWE.
    -- Distinct from the mTLS client certificate, which the connection manager holds.
    signingPrivateKey :: EncryptedField 'AsEncrypted Text,
    -- | HDFC's public key, PEM encoded, used to encrypt the JWE.
    bankPublicKey :: Text,
    -- | Names the TLS manager registered on the runtime; passed to 'callAPI'' as a 'ManagerSelector'.
    tlsManagerKey :: Text,
    -- | HDFC's own cap is 500. We send fewer; see the payout module design.
    maxItemsPerBatch :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
