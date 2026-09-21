{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Wire-shape tests for the Xyne Spaces ticket read API types.
--
-- The encode tests pin the omit-null discipline: Xyne's Zod schemas reject
-- @null@ for absent optionals, so a key must either carry a value or not
-- exist. The decode tests replay the field sets observed on live
-- @list\/search@, ticket-detail and conversation responses (captured
-- 2026-09-11\/15), including unknown extra fields, which must be ignored.
module XyneSpacesTypes where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import Kernel.External.Ticket.XyneSpaces.Types
import Kernel.Prelude
import Test.Tasty
import Test.Tasty.HUnit

txt :: Text -> Text
txt = identity

txts :: [Text] -> [Text]
txts = identity

minimalReq :: XyneListTicketsReq
minimalReq = mkXyneListTicketsReq "ch_desk" 20

minimalEncodes :: TestTree
minimalEncodes =
  testCase "minimal request carries only scope + limit, no null keys" $
    A.toJSON minimalReq
      @?= object ["channelId" .= txt "ch_desk", "limit" .= (20 :: Int)]

filtersEncodeAsArrays :: TestTree
filtersEncodeAsArrays =
  testCase "filters nest under \"filters\" and are list-typed" $ do
    let fs =
          (emptyXyneListFilters :: XyneListFilters)
            { priority = Just [HIGH],
              stageName = Just ["BACKLOG"]
            }
        req =
          minimalReq
            { senderEmail = Just "sender@example.com",
              cursor = Just "opaque-cursor",
              filters = Just fs
            }
    A.toJSON req
      @?= object
        [ "channelId" .= txt "ch_desk",
          "limit" .= (20 :: Int),
          "senderEmail" .= txt "sender@example.com",
          "cursor" .= txt "opaque-cursor",
          "filters"
            .= object
              [ "priority" .= txts ["HIGH"],
                "stageName" .= txts ["BACKLOG"]
              ]
        ]

emptyOptionalsOmitted :: TestTree
emptyOptionalsOmitted =
  testCase "all-Nothing filters, empty boardIds and empty customFields are dropped" $ do
    let req =
          minimalReq
            { filters = Just emptyXyneListFilters,
              boardIds = Just [],
              customFields = Just Map.empty
            }
    A.toJSON req @?= A.toJSON minimalReq

-- Field set observed on a live list/search response (2026-09-11), plus an
-- unknown field that must be ignored.
listRespFixture :: A.Value
listRespFixture =
  object
    [ "items"
        .= [ object
               [ "ticketId" .= txt "cmfgul0f9000123abcd",
                 "xyneId" .= txt "NY-101",
                 "title" .= txt "[P2] [Control Center] Fares & Charges - Fare differs",
                 "statusV2" .= txt "TODO",
                 "stageName" .= txt "BACKLOG",
                 "priority" .= txt "HIGH",
                 "createdAt" .= txt "2026-09-11T10:00:00.000Z",
                 "lastEmailAt" .= txt "2026-09-12T08:30:00.000Z",
                 "conversationId" .= txt "cmfconv001",
                 "channelId" .= txt "ch_desk",
                 "boardId" .= txt "board_1",
                 "projectId" .= txt "proj_1",
                 "customFormData" .= object ["mid" .= txt "MID-88213"],
                 "someFutureField" .= True
               ]
           ],
      "hasMore" .= True,
      "nextCursor" .= txt "eyJvZmZzZXQiOjEwMH0="
    ]

decodesListResp :: TestTree
decodesListResp =
  testCase "list/search response decodes, unknown fields ignored" $
    case A.fromJSON listRespFixture :: A.Result XyneListTicketsResp of
      A.Error err -> assertFailure err
      A.Success resp -> do
        map (\i -> i.ticketId) resp.items @?= [Just "cmfgul0f9000123abcd"]
        map (\i -> i.xyneId) resp.items @?= [Just "NY-101"]
        map (\i -> i.stageName) resp.items @?= [Just "BACKLOG"]
        resp.hasMore @?= Just True
        resp.nextCursor @?= Just "eyJvZmZzZXQiOjEwMH0="

-- Field set observed on a live GET ticket/{id} response (2026-09-11):
-- description, metadata.reporterEmail, emailCount and a history[] trail.
detailFixture :: A.Value
detailFixture =
  object
    [ "id" .= txt "cmfgul0f9000123abcd",
      "xyneId" .= txt "NY-101",
      "title" .= txt "[P2] [Control Center] Fares & Charges - Fare differs",
      "description" .= txt "<p>Reported fare mismatch</p>",
      "statusV2" .= txt "TODO",
      "stageName" .= txt "BACKLOG",
      "priority" .= txt "HIGH",
      "createdAt" .= txt "2026-09-11T10:00:00.000Z",
      "updatedAt" .= txt "2026-09-12T09:00:00.000Z",
      "emailCount" .= (3 :: Int),
      "metadata"
        .= object
          [ "reporterEmail" .= txt "reporter@example.com",
            "fromEmailAddress" .= txt "desk@example.com",
            "extraMetadataField" .= txt "ignored"
          ],
      "history"
        .= [ object
               [ "id" .= txt "hist_1",
                 "activityType" .= txt "TICKET_CREATED",
                 "timestamp" .= txt "2026-09-11T10:00:00.000Z",
                 "updatedByUser" .= object ["name" .= txt "Desk Bot"],
                 "value" .= object ["oldValue" .= A.Null, "newValue" .= txt "BACKLOG"]
               ]
           ]
    ]

decodesTicketDetail :: TestTree
decodesTicketDetail =
  testCase "ticket detail decodes with metadata and raw history" $
    case A.fromJSON detailFixture :: A.Result XyneTicketDetail of
      A.Error err -> assertFailure err
      A.Success detail -> do
        detail.id @?= Just "cmfgul0f9000123abcd"
        detail.emailCount @?= Just 3
        (detail.metadata >>= (\m -> m.reporterEmail)) @?= Just "reporter@example.com"
        fmap length detail.history @?= Just 1

-- Field set observed on a live GET ticket/{id}/conversation response
-- (2026-09-15): raw HTML bodies, externalThreadId on the first message,
-- attachments with originalFilename/mimetype/size.
conversationFixture :: A.Value
conversationFixture =
  object
    [ "items"
        .= [ object
               [ "id" .= txt "msg_1",
                 "from" .= txt "reporter@example.com",
                 "createdAt" .= txt "2026-09-11T10:00:00.000Z",
                 "subject" .= txt "[P2] [Control Center] Fares & Charges - Fare differs",
                 "externalThreadId" .= txt "cc-report-42",
                 "body" .= txt "<p><b>Category:</b> Fares</p>",
                 "attachments"
                   .= [ object
                          [ "id" .= txt "att_1",
                            "originalFilename" .= txt "screenshot.png",
                            "mimetype" .= txt "image/png",
                            "size" .= (204800 :: Int)
                          ]
                      ]
               ]
           ],
      "hasMore" .= False
    ]

decodesConversation :: TestTree
decodesConversation =
  testCase "conversation decodes with raw HTML body and attachments" $
    case A.fromJSON conversationFixture :: A.Result XyneConversationResp of
      A.Error err -> assertFailure err
      A.Success conv -> do
        map (\m -> m.externalThreadId) conv.items @?= [Just "cc-report-42"]
        map (\m -> m.body) conv.items @?= [Just "<p><b>Category:</b> Fares</p>"]
        let atts = concat (mapMaybe (\m -> m.attachments) conv.items)
        map (\a -> a.originalFilename) atts @?= [Just "screenshot.png"]
        map (\a -> a.mimetype) atts @?= [Just "image/png"]
        map (\a -> a.size) atts @?= [Just 204800]

xyneSpacesTypeTests :: TestTree
xyneSpacesTypeTests =
  testGroup
    "Xyne Spaces wire types"
    [ minimalEncodes,
      filtersEncodeAsArrays,
      emptyOptionalsOmitted,
      decodesListResp,
      decodesTicketDetail,
      decodesConversation
    ]
