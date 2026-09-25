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

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.External.Payout.Types (BulkStatusCheckPlan)
import Kernel.Prelude

-- | Connection and identity for HDFC CBX bulk payouts. Carried by the @HdfcCbxConfig@
-- constructor of 'PayoutServiceConfig' and stored per merchant operating city.
data HdfcCbxConfig = HdfcCbxConfig
  { -- | API host, e.g. https://api.hdfcuat.bank.in
    url :: BaseUrl,
    -- | OAuth token endpoint. Two HDFC documents disagree on host and version; confirm before use.
    tokenUrl :: BaseUrl,
    -- | OAuth client credentials, sent as HTTP Basic on the token call. Both halves are
    -- encrypted: the key is as much a credential as the secret, and HDFC issues them as a pair.
    consumerKey :: EncryptedField 'AsEncrypted Text,
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
    -- | Our client certificate, PEM. Whitelisted by the bank against this CBX domain, so it
    -- belongs with the rest of the App's credentials rather than on the host: an environment
    -- can then be configured entirely from this row, and two cities on different certs can
    -- share one deployment. Public material -- it is the certificate we present.
    clientCertPem :: Text,
    -- | The bank's CA chain, PEM, used to verify their server. Public.
    caBundlePem :: Text,
    -- | Private key for the client certificate above. Encrypted, as 'signingPrivateKey' is.
    -- Distinct from that key: this one proves the connection, that one proves the message.
    clientKeyPem :: EncryptedField 'AsEncrypted Text,
    -- | HDFC's own cap is 500. We send fewer; see the payout module design.
    maxItemsPerBatch :: Int,
    -- | How often a submitted batch is asked about. Optional so that a row written before
    -- this field existed still decodes -- an absent plan means the shared default, which is
    -- HDFC's own published cadence, so behaviour is unchanged until someone sets one.
    bulkStatusCheckPlan :: Maybe BulkStatusCheckPlan,
    -- | IFSC prefix of the partner's own bank, e.g. @"HDFC"@. A beneficiary whose IFSC starts
    -- with it is paid intra-bank, which has no RBI settlement leg: @rbistatus@ is never
    -- populated for them, so terminality has to be read from @codstatus@ alone.
    --
    -- Needed because the partner converts the rail silently -- a NEFT to a beneficiary who
    -- banks with them is executed intra-bank -- while echoing back the @cdflag@ we SENT, so
    -- the request's own rail cannot reveal it.
    --
    -- Optional so a row written before this field existed still decodes. Absent means no
    -- beneficiary is recognised as intra-bank by IFSC, leaving only the @cdflag@ test, which
    -- is exactly the previous behaviour.
    ownBankIfscPrefix :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Name under which this config's TLS manager is registered on the runtime, and the
-- 'ManagerSelector' used to pick it again at call time.
--
-- Derived rather than configured: registration and lookup both compute it from the same row,
-- so they cannot drift, and a typo cannot produce @HttpManagerNotFound@ at 2am. The domain is
-- kept in the name so a log line identifies the CBX domain at a glance; the hash distinguishes
-- two certificates on the same domain and changes when a certificate is rotated.
--
-- Hashed over the certificate and CA only, never the key: the key is stored as ciphertext and
-- re-encrypting the same key yields different bytes, which would produce a new manager on every
-- reseed of identical material. Two cities sharing a certificate therefore share one manager,
-- and one connection pool, which is what they are to the bank anyway.
hdfcManagerKey :: HdfcCbxConfig -> Text
hdfcManagerKey cfg =
  "hdfc-cbx:" <> cfg.groupId <> ":" <> cfg.clientCode <> ":" <> materialHash
  where
    digest = Hash.hashWith Hash.SHA256 . TE.encodeUtf8 $ cfg.clientCertPem <> cfg.caBundlePem
    materialHash = T.take 12 . TE.decodeUtf8 . BAE.convertToBase BAE.Base16 $ digest
