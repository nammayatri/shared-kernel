module SettlementEmail where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import SettlementMimeCsv (extractCsvFromMime, extractMessageIds) -- not sure about this this one is only for testing
import Test.Tasty
import Test.Tasty.HUnit

settlementEmailTests :: TestTree
settlementEmailTests =
  testGroup
    "Settlement Email Parsing"
    [ extractMessageIdsTests,
      extractCsvFromMimeTests
    ]

extractMessageIdsTests :: TestTree
extractMessageIdsTests =
  testGroup
    "extractMessageIds"
    [ testCase "parses numeric IDs from IMAP SEARCH output" $ do
        let raw = "* SEARCH 101 102 103\r\n"
        extractMessageIds raw @?= ["103", "102", "101"],
      testCase "returns empty for no matches" $ do
        extractMessageIds "* SEARCH\r\n" @?= [],
      testCase "ignores non-numeric tokens" $ do
        extractMessageIds "* SEARCH abc 42 def 99" @?= ["99", "42"]
    ]

extractCsvFromMimeTests :: TestTree
extractCsvFromMimeTests =
  testGroup
    "extractCsvFromMime"
    [ testCase "extracts plain-text CSV from MIME" $ do
        let mime =
              joinMimeLines
                [ "MIME-Version: 1.0",
                  "Content-Type: multipart/mixed; boundary=\"boundary123\"",
                  "",
                  "--boundary123",
                  "Content-Type: text/csv; name=\"report.csv\"",
                  "Content-Transfer-Encoding: 7bit",
                  "",
                  "txn_id,amount,status",
                  "T001,100.00,settled",
                  "T002,200.50,settled",
                  "--boundary123--"
                ]
        case extractCsvFromMime mime of
          Left err -> assertFailure $ "Expected CSV content but got error: " <> toString err
          Right bs -> do
            let csv = TE.decodeUtf8 (LBS.toStrict bs)
            assertBool "should contain txn_id header" ("txn_id" `isInfixOf` csv)
            assertBool "should contain T001 row" ("T001" `isInfixOf` csv),
      testCase "extracts base64-encoded CSV from MIME" $ do
        -- "hello,world\n" base64-encoded = "aGVsbG8sd29ybGQK"
        let mime =
              joinMimeLines
                [ "--boundary",
                  "Content-Type: text/csv; name=\"data.csv\"",
                  "Content-Transfer-Encoding: base64",
                  "",
                  "aGVsbG8sd29ybGQK",
                  "--boundary--"
                ]
        case extractCsvFromMime mime of
          Left err -> assertFailure $ "Expected CSV content but got error: " <> toString err
          Right bs -> do
            let csv = TE.decodeUtf8 (LBS.toStrict bs)
            assertBool "should contain hello,world" ("hello,world" `isInfixOf` csv),
      testCase "returns error when no CSV attachment" $ do
        let mime =
              joinMimeLines
                [ "Content-Type: text/plain",
                  "",
                  "Just a plain email body"
                ]
        case extractCsvFromMime mime of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected error for non-CSV email"
    ]

-- | Join lines with newline separators; no trailing newline after the last line.
joinMimeLines :: [Text] -> Text
joinMimeLines = T.intercalate "\n"
