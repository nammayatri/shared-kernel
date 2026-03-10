module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Kernel.External.Settlement.Interface
import Kernel.Prelude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pgStr, _listOrType] -> do
      pg <- parsePG pgStr
      listSamples pg
    [pgStr, reportType, csvFile] -> do
      pg <- parsePG pgStr
      let pgDir = pgDirName pg
          csvPath = resolveCsvPath pgDir csvFile
          outDir = "output" </> pgDir
      liftIO $ createDirectoryIfMissing True outDir
      exists <- liftIO $ doesFileExist csvPath
      if not exists
        then do
          liftIO $ TIO.putStrLn $ "File not found: " <> T.pack csvPath
          liftIO $ TIO.putStrLn $ "Looked in: samples/" <> T.pack pgDir <> "/"
          liftIO $ TIO.putStrLn ""
          listSamples pg
          liftIO exitFailure
        else do
          csvData <- liftIO $ LBS.readFile csvPath
          let outputPath = outDir </> takeFileName csvPath <> ".parsed.txt"
          case reportType of
            "payment" -> do
              let result = parsePaymentSettlementCsv pg csvData
              writeResult outputPath (formatPaymentResult result)
            "payout" -> do
              let result = parsePayoutSettlementCsv pg csvData
              writeResult outputPath (formatPayoutResult result)
            _ -> do
              liftIO $ TIO.putStrLn "Unknown report type. Use 'payment' or 'payout'."
              liftIO exitFailure
    _ -> usage

parsePG :: String -> IO SettlementService
parsePG s = case map Char.toLower s of
  "hyperpg" -> pure HyperPG
  "billdesk" -> pure BillDesk
  _ -> do
    liftIO $ TIO.putStrLn $ "Unknown PG: " <> T.pack s
    liftIO $ TIO.putStrLn "Supported PGs: hyperpg, billdesk"
    liftIO exitFailure

pgDirName :: SettlementService -> FilePath
pgDirName HyperPG = "hyperpg"
pgDirName BillDesk = "billdesk"

resolveCsvPath :: FilePath -> String -> FilePath
resolveCsvPath pgDir csvFile =
  if '/' `elem` csvFile
    then csvFile
    else "samples" </> pgDir </> csvFile

listSamples :: SettlementService -> IO ()
listSamples pg = do
  let dir = "samples" </> pgDirName pg
  dirExists <- liftIO $ doesDirectoryExist dir
  if not dirExists
    then liftIO $ TIO.putStrLn $ "No samples directory found at " <> T.pack dir <> "/"
    else do
      files <- liftIO $ listDirectory dir
      let csvFiles = filter (\f -> ".csv" `isSuffixOf` f) files
      if null csvFiles
        then liftIO $ TIO.putStrLn $ "No sample CSVs found in " <> T.pack dir <> "/"
        else do
          liftIO $ TIO.putStrLn $ "Available samples in " <> T.pack dir <> "/:"
          mapM_ (\f -> liftIO $ TIO.putStrLn $ "  " <> T.pack f) csvFiles

usage :: IO ()
usage = do
  liftIO $ TIO.putStrLn "test-settlement-parser -- local CSV parsing test tool"
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "Usage:"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- <pg> <payment|payout> <csv-file>"
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "  <pg>         Payment gateway: hyperpg | billdesk"
  liftIO $ TIO.putStrLn "  <csv-file>   Filename in samples/<pg>/ or a relative/absolute path"
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "Examples:"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- hyperpg payment metropol_de6c3402_YESAP60685556165_20260309.csv"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- hyperpg payout cumta_tab_performance_table.csv"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- billdesk payment billdesk_settlement_report.csv"
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "List samples for a PG:"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- hyperpg list"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- billdesk list"
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "Output: output/<pg>/<filename>.parsed.txt"
  liftIO exitFailure

writeResult :: FilePath -> Text -> IO ()
writeResult outputPath content = do
  TIO.writeFile outputPath content
  liftIO $ TIO.putStrLn $ T.pack $ "Output written to: " <> outputPath
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "--- Preview (first 80 lines) ---"
  let lns = T.lines content
      preview = T.unlines (take 80 lns)
  TIO.putStrLn preview
  if length lns > 80
    then liftIO $ TIO.putStrLn $ "... (" <> show (length lns - 80) <> " more lines in file)"
    else pure ()

-- ---------------------------------------------------------------------------
-- Payment report formatting
-- ---------------------------------------------------------------------------

formatPaymentResult :: ParsePaymentSettlementResult -> Text
formatPaymentResult result =
  let hdr =
        T.unlines
          [ "========================================",
            " Payment Settlement Parse Result",
            "========================================",
            "Total rows:    " <> show (totalRows result),
            "Parsed OK:     " <> show (totalRows result - failedRows result),
            "Failed rows:   " <> show (failedRows result),
            ""
          ]
      errSection =
        if null (errors result)
          then "No parsing errors.\n\n"
          else
            T.unlines $
              ["--- Parsing Errors ---"]
                <> map ("  * " <>) (errors result)
                <> [""]
      reportLines = T.unlines $ zipWith fmtPaymentRow [1 :: Int ..] (reports result)
   in hdr <> errSection <> reportLines

fmtPaymentRow :: Int -> PaymentSettlementReport -> Text
fmtPaymentRow idx r =
  T.unlines
    [ "--- Report #" <> show idx <> " ---",
      fld "orderId" r.orderId,
      opt "txnId" r.txnId,
      fld "txnType" (show r.txnType),
      fld "txnStatus" (show r.txnStatus),
      optT "txnDate" r.txnDate,
      fld "txnAmount" (show r.txnAmount),
      fld "pgBaseFee" (show r.pgBaseFee),
      fld "pgTax" (show r.pgTax),
      fld "settlementAmount" (show r.settlementAmount),
      fld "currency" (show r.currency),
      opt "vendorId" r.vendorId,
      opt "uniqueSplitId" r.uniqueSplitId,
      opt "paymentGateway" r.paymentGateway,
      optS "paymentMethod" r.paymentMethod,
      opt "paymentMethodSubType" r.paymentMethodSubType,
      optS "settlementType" r.settlementType,
      optS "settlementMode" r.settlementMode,
      opt "settlementId" r.settlementId,
      optT "settlementDate" r.settlementDate,
      opt "rrn" r.rrn,
      opt "utr" r.utr,
      opt "refundId" r.refundId,
      optT "refundDate" r.refundDate,
      optS "refundAmount" r.refundAmount,
      opt "disputeId" r.disputeId,
      opt "cardIsin" r.cardIsin,
      opt "cardNetwork" r.cardNetwork,
      opt "cardType" r.cardType
    ]

-- ---------------------------------------------------------------------------
-- Payout report formatting
-- ---------------------------------------------------------------------------

formatPayoutResult :: ParsePayoutSettlementResult -> Text
formatPayoutResult result =
  let hdr =
        T.unlines
          [ "========================================",
            " Payout Settlement Parse Result",
            "========================================",
            "Total rows:    " <> show (totalRows result),
            "Parsed OK:     " <> show (totalRows result - failedRows result),
            "Failed rows:   " <> show (failedRows result),
            ""
          ]
      errSection =
        if null (errors result)
          then "No parsing errors.\n\n"
          else
            T.unlines $
              ["--- Parsing Errors ---"]
                <> map ("  * " <>) (errors result)
                <> [""]
      reportLines = T.unlines $ zipWith fmtPayoutRow [1 :: Int ..] (reports result)
   in hdr <> errSection <> reportLines

fmtPayoutRow :: Int -> PayoutSettlementReport -> Text
fmtPayoutRow idx r =
  T.unlines
    [ "--- Report #" <> show idx <> " ---",
      fld "orderId" r.orderId,
      opt "txnId" r.txnId,
      fld "txnStatus" (show r.txnStatus),
      optT "txnDate" r.txnDate,
      fld "txnAmount" (show r.txnAmount),
      fld "settlementAmount" (show r.settlementAmount),
      fld "currency" (show r.currency),
      fld "payoutCustomerId" r.payoutCustomerId,
      opt "paymentGateway" r.paymentGateway,
      opt "beneficiaryIfsc" r.beneficiaryIfsc,
      opt "beneficiaryAccount" r.beneficiaryAccountNumber,
      opt "beneficiaryType" r.beneficiaryType,
      opt "bankName" r.bankName,
      optS "settlementType" r.settlementType,
      optS "settlementMode" r.settlementMode,
      opt "settlementId" r.settlementId,
      optT "settlementDate" r.settlementDate,
      opt "fulfillmentId" r.fulfillmentId,
      optS "fulfillmentInstrument" r.fulfillmentInstrumentType,
      opt "fulfillmentMethod" r.fulfillmentMethod,
      opt "fulfillmentStatus" r.fulfillmentStatus,
      optT "fulfillmentDate" r.fulfillmentDate,
      optS "fulfillmentAmount" r.fulfillmentAmount,
      opt "rrn" r.rrn,
      opt "utr" r.utr,
      opt "responseCode" r.fulfillmentResponseCode,
      opt "responseMessage" r.fulfillmentResponseMessage
    ]

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

fld :: Text -> Text -> Text
fld label value = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> value

opt :: Text -> Maybe Text -> Text
opt label Nothing = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> "<empty>"
opt label (Just v) = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> v

optS :: (Show a) => Text -> Maybe a -> Text
optS label Nothing = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> "<empty>"
optS label (Just v) = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> show v

optT :: Text -> Maybe UTCTime -> Text
optT label Nothing = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> "<empty>"
optT label (Just v) = "  " <> T.justifyLeft 26 ' ' (label <> ":") <> show v
