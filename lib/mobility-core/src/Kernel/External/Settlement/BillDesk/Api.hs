{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.BillDesk.Api
  ( ApplicationJose,
    JoseResponse (..),
    RetrieveSettlementAPI,
    retrieveSettlementAPI,
    retrieveSettlementClient,
    RetrieveSettlementDetailsAPI,
    retrieveSettlementDetailsAPI,
    retrieveSettlementDetailsClient,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified EulerHS.Types as ET
import Kernel.Prelude
import qualified Network.HTTP.Media as M
import Servant

-- ---------------------------------------------------------------------------
-- Custom content type: application/jose
-- ---------------------------------------------------------------------------

data ApplicationJose deriving (Typeable)

instance Accept ApplicationJose where
  contentType _ = "application" M.// "jose"

instance MimeRender ApplicationJose LBS.ByteString where
  mimeRender _ = identity

instance MimeUnrender ApplicationJose JoseResponse where
  mimeUnrender _ bs = Right (JoseResponse bs)

-- ---------------------------------------------------------------------------
-- JoseResponse: newtype with ToJSON for callAPI logging
-- ---------------------------------------------------------------------------

newtype JoseResponse = JoseResponse {unJoseResponse :: LBS.ByteString}

instance A.ToJSON JoseResponse where
  toJSON (JoseResponse bs) = A.String (decodeUtf8 $ LBS.toStrict bs)

-- ---------------------------------------------------------------------------
-- Retrieve Settlement API
-- ---------------------------------------------------------------------------

type RetrieveSettlementAPI =
  "pasettlements" :> "v1_2" :> "settlements" :> "v2" :> "getsettlement"
    :> Header "BD-Traceid" Text
    :> Header "BD-Timestamp" Text
    :> ReqBody '[ApplicationJose] LBS.ByteString
    :> Post '[ApplicationJose] JoseResponse

retrieveSettlementAPI :: Proxy RetrieveSettlementAPI
retrieveSettlementAPI = Proxy

retrieveSettlementClient ::
  Maybe Text ->
  Maybe Text ->
  LBS.ByteString ->
  ET.EulerClient JoseResponse
retrieveSettlementClient = ET.client retrieveSettlementAPI

-- ---------------------------------------------------------------------------
-- Retrieve Settlement Details API
-- ---------------------------------------------------------------------------

type RetrieveSettlementDetailsAPI =
  "pasettlements" :> "v1_2" :> "settlements" :> "v2" :> "getsettlementDetails"
    :> Header "BD-Traceid" Text
    :> Header "BD-Timestamp" Text
    :> ReqBody '[ApplicationJose] LBS.ByteString
    :> Post '[ApplicationJose] JoseResponse

retrieveSettlementDetailsAPI :: Proxy RetrieveSettlementDetailsAPI
retrieveSettlementDetailsAPI = Proxy

retrieveSettlementDetailsClient ::
  Maybe Text ->
  Maybe Text ->
  LBS.ByteString ->
  ET.EulerClient JoseResponse
retrieveSettlementDetailsClient = ET.client retrieveSettlementDetailsAPI
