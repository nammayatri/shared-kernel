{-
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Kernel.External.Payout.Juspay.Config where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Kernel.Utils.Jose as Jose

data JuspayConfig = JuspayConfig
  { apiKey :: EncryptedField 'AsEncrypted Text,
    returnUrl :: BaseUrl,
    url :: BaseUrl,
    merchantId :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    gatewayReferenceId :: Maybe Text,
    dynamicWebhookUrl :: Maybe Text,
    pgBaseFee :: Maybe HighPrecMoney,
    pgGstRate :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data JuspayPayoutEncryptionConfig = JuspayPayoutEncryptionConfig
  { -- | Juspay's RSA public key, PEM encoded, stored passetto-encrypted like 'merchantPrivateKey'.
    -- The request JWE is encrypted to it.
    juspayPublicKey :: EncryptedField 'AsEncrypted Text,
    -- | Juspay's identifier for its key pair (the guide's @kid@). Sent as @kid@ in the JWE
    -- header and in the body.
    juspayKeyId :: Text,
    -- | Juspay's identifier for our public key (the guide's @sid@). Sent as @sid@ in the body
    -- and as @kid@ in the JWS header.
    merchantKeyId :: Text,
    -- | Our RSA private key, PEM encoded, whose public half was shared with Juspay. Signs the
    -- request. Required unless 'signRequest' is @false@.
    merchantPrivateKey :: Maybe (EncryptedField 'AsEncrypted Text),
    -- | Sign the JWE with 'merchantPrivateKey' (RS256, after encryption, step 3 of the guide),
    -- so @payload@ is a compact JWS whose payload is the JWE. Defaults to @true@, as the guide
    -- describes; set @false@ to send the bare JWE, as Juspay's order-create sample does.
    signRequest :: Maybe Bool,
    -- | JWE key wrapping. Defaults to @RSA-OAEP-256@, which the guide names; Juspay's own
    -- samples use @RSA-OAEP@, so set that if the account is provisioned for it.
    keyWrapAlgorithm :: Maybe Jose.JweKeyAlg,
    -- | JWE content encryption. Defaults to @A128CBC-HS256@, which the guide names; Juspay's
    -- attempt-API sample uses @A256CBC-HS512@, which is also supported.
    contentEncryption :: Maybe Jose.JweContentEnc
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
