{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.External.SAP.Types where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))
import Text.XML (def, parseText)
import Text.XML.Cursor (content, fromDocument, laxElement, ($//), (&/))
import qualified Text.XML.Cursor as XC

-- OAuth token response

data SAPTokenResp = SAPTokenResp
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int,
    scope :: Maybe Text,
    jti :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Journal entry types

data SAPJournalHeader = SAPJournalHeader
  { msgtyp :: Maybe Text,
    batchId :: Text,
    requestDate :: Text,
    requestTime :: Text,
    headerdesc :: Text,
    bukrs :: Text,
    blart :: Text,
    budat :: Text,
    bldat :: Text,
    attrName1 :: Maybe Text,
    attrValue1 :: Maybe Text,
    attrName2 :: Maybe Text,
    attrValue2 :: Maybe Text,
    attrName3 :: Maybe Text,
    attrValue3 :: Maybe Text,
    attrName4 :: Maybe Text,
    attrValue4 :: Maybe Text,
    attrName5 :: Maybe Text,
    attrValue5 :: Maybe Text,
    belnr :: Maybe Text,
    gjahr :: Maybe Text,
    message :: Maybe Text,
    items :: [SAPJournalItem]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SAPJournalItem = SAPJournalItem
  { batchId :: Text,
    batchItem :: Text,
    itemdesc :: Text,
    hkont :: Text,
    amount :: Text,
    shkzg :: Text,
    kostl :: Maybe Text,
    prctr :: Maybe Text,
    waers :: Text,
    attrName1 :: Maybe Text,
    attrValue1 :: Maybe Text,
    attrName2 :: Maybe Text,
    attrValue2 :: Maybe Text,
    attrName3 :: Maybe Text,
    attrValue3 :: Maybe Text,
    attrName4 :: Maybe Text,
    attrValue4 :: Maybe Text,
    attrName5 :: Maybe Text,
    attrValue5 :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype SAPJournalRequest = SAPJournalRequest
  { headers :: [SAPJournalHeader]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

renderXmlElement :: Text -> Maybe Text -> Text
renderXmlElement tag = \case
  Nothing -> "<" <> tag <> "></" <> tag <> ">"
  Just val -> "<" <> tag <> ">" <> val <> "</" <> tag <> ">"

renderItem :: SAPJournalItem -> Text
renderItem item =
  T.concat
    [ "<Item>",
      renderXmlElement "BatchId" (Just item.batchId),
      renderXmlElement "BatchItem" (Just item.batchItem),
      renderXmlElement "Itemdesc" (Just item.itemdesc),
      renderXmlElement "Hkont" (Just item.hkont),
      renderXmlElement "Amount" (Just item.amount),
      renderXmlElement "Shkzg" (Just item.shkzg),
      renderXmlElement "Kostl" item.kostl,
      renderXmlElement "Prctr" item.prctr,
      renderXmlElement "Waers" (Just item.waers),
      renderXmlElement "AttrName1" item.attrName1,
      renderXmlElement "AttrValue1" item.attrValue1,
      renderXmlElement "AttrName2" item.attrName2,
      renderXmlElement "AttrValue2" item.attrValue2,
      renderXmlElement "AttrName3" item.attrName3,
      renderXmlElement "AttrValue3" item.attrValue3,
      renderXmlElement "AttrName4" item.attrName4,
      renderXmlElement "AttrValue4" item.attrValue4,
      renderXmlElement "AttrName5" item.attrName5,
      renderXmlElement "AttrValue5" item.attrValue5,
      "</Item>"
    ]

renderHeader :: SAPJournalHeader -> Text
renderHeader hdr =
  T.concat
    [ "<Header>",
      renderXmlElement "Msgtyp" hdr.msgtyp,
      renderXmlElement "BatchId" (Just hdr.batchId),
      renderXmlElement "RequestDate" (Just hdr.requestDate),
      renderXmlElement "RequestTime" (Just hdr.requestTime),
      renderXmlElement "Headerdesc" (Just hdr.headerdesc),
      renderXmlElement "Bukrs" (Just hdr.bukrs),
      renderXmlElement "Blart" (Just hdr.blart),
      renderXmlElement "Budat" (Just hdr.budat),
      renderXmlElement "Bldat" (Just hdr.bldat),
      renderXmlElement "AttrName1" hdr.attrName1,
      renderXmlElement "AttrValue1" hdr.attrValue1,
      renderXmlElement "AttrName2" hdr.attrName2,
      renderXmlElement "AttrValue2" hdr.attrValue2,
      renderXmlElement "AttrName3" hdr.attrName3,
      renderXmlElement "AttrValue3" hdr.attrValue3,
      renderXmlElement "AttrName4" hdr.attrName4,
      renderXmlElement "AttrValue4" hdr.attrValue4,
      renderXmlElement "AttrName5" hdr.attrName5,
      renderXmlElement "AttrValue5" hdr.attrValue5,
      renderXmlElement "Belnr" hdr.belnr,
      renderXmlElement "Gjahr" hdr.gjahr,
      renderXmlElement "Message" hdr.message,
      renderXmlElement "Msgtyp" hdr.msgtyp,
      "<ItemSet>",
      T.concat (map renderItem hdr.items),
      "</ItemSet>",
      "</Header>"
    ]

renderJournalRequestXml :: SAPJournalRequest -> BSL.ByteString
renderJournalRequestXml req =
  BSL.fromStrict . TE.encodeUtf8 $
    T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        "<HeaderSet>",
        T.concat (map renderHeader req.headers),
        "</HeaderSet>"
      ]

-- Response types

data SAPJournalResponse = SAPJournalResponse
  { responseHeaders :: [SAPJournalHeaderResponse],
    rawXml :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SAPJournalHeaderResponse = SAPJournalHeaderResponse
  { msgtyp :: Maybe Text,
    batchId :: Maybe Text,
    requestDate :: Maybe Text,
    requestTime :: Maybe Text,
    headerdesc :: Maybe Text,
    bukrs :: Maybe Text,
    blart :: Maybe Text,
    budat :: Maybe Text,
    bldat :: Maybe Text,
    belnr :: Maybe Text,
    gjahr :: Maybe Text,
    message :: Maybe Text,
    attrName1 :: Maybe Text,
    attrValue1 :: Maybe Text,
    attrName2 :: Maybe Text,
    attrValue2 :: Maybe Text,
    attrName3 :: Maybe Text,
    attrValue3 :: Maybe Text,
    attrName4 :: Maybe Text,
    attrValue4 :: Maybe Text,
    attrName5 :: Maybe Text,
    attrValue5 :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

parseSAPJournalResponse :: BSL.ByteString -> Either String SAPJournalResponse
parseSAPJournalResponse bs =
  case TE.decodeUtf8' (BSL.toStrict bs) of
    Left err -> Left (show err)
    Right txt -> case parseText def (TL.fromStrict txt) of
      Left xmlErr -> Left (show xmlErr)
      Right doc ->
        let cursor = fromDocument doc
            headerCursors = cursor $// laxElement "Header"
            parsedHeaders = map parseHeaderResponse headerCursors
         in Right
              SAPJournalResponse
                { responseHeaders = parsedHeaders,
                  rawXml = txt
                }

parseHeaderResponse :: XC.Cursor -> SAPJournalHeaderResponse
parseHeaderResponse c =
  SAPJournalHeaderResponse
    { msgtyp = lookupField c "Msgtyp",
      batchId = lookupField c "BatchId",
      requestDate = lookupField c "RequestDate",
      requestTime = lookupField c "RequestTime",
      headerdesc = lookupField c "Headerdesc",
      bukrs = lookupField c "Bukrs",
      blart = lookupField c "Blart",
      budat = lookupField c "Budat",
      bldat = lookupField c "Bldat",
      belnr = lookupField c "Belnr",
      gjahr = lookupField c "Gjahr",
      message = lookupField c "Message",
      attrName1 = lookupField c "AttrName1",
      attrValue1 = lookupField c "AttrValue1",
      attrName2 = lookupField c "AttrName2",
      attrValue2 = lookupField c "AttrValue2",
      attrName3 = lookupField c "AttrName3",
      attrValue3 = lookupField c "AttrValue3",
      attrName4 = lookupField c "AttrName4",
      attrValue4 = lookupField c "AttrValue4",
      attrName5 = lookupField c "AttrName5",
      attrValue5 = lookupField c "AttrValue5"
    }

lookupField :: XC.Cursor -> Text -> Maybe Text
lookupField cursor fieldName =
  case cursor $// laxElement fieldName &/ content of
    [] -> Nothing
    ts ->
      let val = T.strip (T.concat ts)
       in if T.null val then Nothing else Just val

-- Error types

data SAPError
  = SAPBadRequest
  | SAPUnauthorized
  | SAPForbidden
  | SAPServerError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SAPError

instance IsBaseError SAPError where
  toMessage = \case
    SAPBadRequest -> Just "SAP: Bad request."
    SAPUnauthorized -> Just "SAP: Unauthorized, invalid credentials."
    SAPForbidden -> Just "SAP: Forbidden."
    SAPServerError -> Just "SAP: Something went wrong."

instance IsHTTPError SAPError where
  toErrorCode = \case
    SAPBadRequest -> "SAP_BAD_REQUEST"
    SAPUnauthorized -> "SAP_UNAUTHORIZED"
    SAPForbidden -> "SAP_FORBIDDEN"
    SAPServerError -> "SAP_SERVER_ERROR"

  toHttpCode = \case
    SAPBadRequest -> E400
    SAPUnauthorized -> E401
    SAPForbidden -> E403
    SAPServerError -> E500

instance IsAPIError SAPError

instance FromResponse SAPError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just SAPBadRequest
    401 -> Just SAPUnauthorized
    403 -> Just SAPForbidden
    _ -> Just SAPServerError
