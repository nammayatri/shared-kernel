{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.SAP.API
  ( SAPTokenAPI,
    sapTokenAPI,
    SAPJournalPostAPI,
    sapJournalPostAPI,
    SAPApplicationXML,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Kernel.External.SAP.Types (SAPJournalResponse, SAPTokenResp, parseSAPJournalResponse)
import Kernel.Prelude
import qualified Network.HTTP.Media as M
import Servant

-- OAuth token endpoint

type SAPTokenAPI =
  "oauth2"
    :> "api"
    :> "v1"
    :> "token"
    :> QueryParam "grant_type" Text
    :> QueryParam "token_format" Text
    :> Header "Authorization" Text
    :> Post '[JSON] SAPTokenResp

sapTokenAPI :: Proxy SAPTokenAPI
sapTokenAPI = Proxy

-- XML content type for SAP

data SAPApplicationXML deriving (Typeable)

instance Accept SAPApplicationXML where
  contentType _ = "application" M.// "xml"

instance MimeRender SAPApplicationXML BSL.ByteString where
  mimeRender _ = identity

instance MimeUnrender SAPApplicationXML SAPJournalResponse where
  mimeUnrender _ = parseSAPJournalResponse

-- Journal entry posting endpoint

type SAPJournalPostAPI =
  "http"
    :> "aws"
    :> "s4"
    :> "odata"
    :> "ondc"
    :> Header "Authorization" Text
    :> ReqBody '[SAPApplicationXML] BSL.ByteString
    :> Post '[SAPApplicationXML] SAPJournalResponse

sapJournalPostAPI :: Proxy SAPJournalPostAPI
sapJournalPostAPI = Proxy
