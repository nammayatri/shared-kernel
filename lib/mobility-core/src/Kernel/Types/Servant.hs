{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Servant
  ( PlainText_ISO_8859_1,
    RawByteString (..),
    RawJson,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import Data.OpenApi (NamedSchema (NamedSchema), ToSchema (..), byteSchema)
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import qualified Network.HTTP.Media as M
import Servant
import qualified Servant.API as S

data PlainText_ISO_8859_1 deriving (Typeable)

instance Accept PlainText_ISO_8859_1 where
  contentType _ = "text" M.// "plain" M./: ("charset", "ISO-8859-1")

-- | OpenApi/Servant wrapper so inbound webhook routes can receive raw bytes for
-- signature verification (JSON parsing happens after verify).
newtype RawByteString = RawByteString {getRawByteString :: LBS.ByteString}

instance ToSchema RawByteString where
  declareNamedSchema _ = pure $ NamedSchema (Just "RawByteString") byteSchema

instance S.MimeRender OctetStream RawByteString where
  mimeRender _ = getRawByteString

instance S.MimeUnrender OctetStream RawByteString where
  mimeUnrender _ = Right . RawByteString

-- | Content type that advertises @application/json@ on the wire while keeping
-- the body as raw bytes for HMAC verification.
--
-- Defining a fresh 'Accept'-tagged type avoids the overlap with Servant's
-- generic @FromJSON a => MimeUnrender JSON a@ instance.
data RawJson

instance Accept RawJson where
  contentTypes _ = NE.fromList ["application" M.// "json", "application" M.// "*"]

instance S.MimeRender RawJson RawByteString where
  mimeRender _ = getRawByteString

instance S.MimeUnrender RawJson RawByteString where
  mimeUnrender _ = Right . RawByteString
