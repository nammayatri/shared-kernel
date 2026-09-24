{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Error.BaseError.HTTPError.BecknAPIError
  ( module Kernel.Types.Error.BaseError.HTTPError.BecknAPIError,
    Error.Error (..),
    Error.ErrorType (..),
  )
where

import Data.Aeson.Types
import EulerHS.Prelude hiding (Show, show)
import qualified Kernel.Types.Beckn.Error as Error
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Prelude (Show (..))

class IsBecknAPIError e where
  toType :: e -> Error.ErrorType
  toType _ = Error.INTERNAL_ERROR

  toPath :: e -> Maybe Text
  toPath _ = Nothing

  toOndcErrorCode :: e -> Maybe Text
  toOndcErrorCode _ = Nothing

newtype BecknAPIError = BecknAPIError Error.Error
  deriving (Generic, Eq, Show)

-- ONDC TRV10 v2.1.0 shape: `{message: {ack: {status: "NACK"}}, error: {code, message}}`.
-- The error object carries only `code` and `message` — internal `type` and `path`
-- remain in the Haskell Error record for logs/metrics but are not emitted. Optional
-- `"response"` wrapping for deployments that need it is handled by WAI middleware at
-- the HTTP edge, not here.
instance FromJSON BecknAPIError where
  parseJSON (Object v) = do
    -- Accept both unwrapped (spec) and wrapped `{response: {...}}` bodies in case a peer
    -- echoes back our wrapped output.
    inner <- (v .: "response") <|> pure v
    errObj <- inner .: "error"
    errCode <- errObj .: "code"
    errMsg <- errObj .:? "message"
    pure $
      BecknAPIError
        Error.Error
          { Error._type = Error.INTERNAL_ERROR,
            Error.code = errCode,
            Error.path = Nothing,
            Error.message = errMsg
          }
  parseJSON invalid =
    prependFailure
      "Parsing BecknAPIError failed, "
      (typeMismatch "Object" invalid)

instance ToJSON BecknAPIError where
  toJSON (BecknAPIError err) =
    object
      [ "message" .= object ["ack" .= object ["status" .= ("NACK" :: Text)]],
        "error"
          .= object
            [ "code" .= Error.code err,
              "message" .= Error.message err
            ]
      ]

instance FromResponse BecknAPIError where
  fromResponse = fromJsonResponse
