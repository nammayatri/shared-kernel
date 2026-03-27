module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Kernel.External.Encryption (Encrypted (..))
import Kernel.External.Settlement.Interface
import Kernel.External.Settlement.Sources.Email (fetchSettlementFileDebugRaw, fetchSettlementFileWithPlainPassword)
import qualified Kernel.External.Settlement.Sources.SFTP as SFTPSource
import Kernel.Prelude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (dropExtension, takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import qualified Prelude as P

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["sftp", pgStr, keyPath, userAtHost, remotePath] -> do
      pg <- parsePG pgStr
      testSftpFetch pg keyPath userAtHost 22 remotePath Nothing
    ["sftp", pgStr, keyPath, userAtHost, remotePath, portStr] -> do
      pg <- parsePG pgStr
      testSftpFetch pg keyPath userAtHost (read portStr) remotePath Nothing
    ["sftp", pgStr, keyPath, userAtHost, remotePath, portStr, pattern_] -> do
      pg <- parsePG pgStr
      testSftpFetch pg keyPath userAtHost (read portStr) remotePath (Just pattern_)
    ["email", pgStr, user, pass] -> do
      pg <- parsePG pgStr
      testEmailFetch pg user pass Nothing
    ["email", pgStr, user, pass, subj] -> do
      pg <- parsePG pgStr
      testEmailFetch pg user pass (Just subj)
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

testEmailFetch :: SettlementService -> String -> String -> Maybe String -> IO ()
testEmailFetch pg user pass mbSubj = do
  let pgDir = pgDirName pg
      samplesDir = "samples" </> pgDir
      outDir = "output" </> pgDir
  liftIO $ createDirectoryIfMissing True samplesDir
  liftIO $ createDirectoryIfMissing True outDir
  liftIO $ TIO.putStrLn $ "Connecting to IMAP server (imap.gmail.com:993) for " <> T.pack pgDir <> " ..."
  let emailConfig =
        EmailConfig
          { imapHost = "imap.gmail.com",
            imapPort = 993,
            username = T.pack user,
            password = Encrypted (T.pack pass), -- dummy, not used
            folderName = "INBOX",
            subjectFilter = T.pack <$> mbSubj
          }
  -- Always dump raw MIME for debugging
  liftIO $ TIO.putStrLn "Fetching raw MIME..."
  debugResult <- fetchSettlementFileDebugRaw emailConfig (T.pack pass)
  case debugResult of
    Nothing -> liftIO $ TIO.putStrLn "(no matching email found — it may already be read/SEEN)"
    Just rawMime -> do
      let debugPath = outDir </> "debug-raw-mime.txt"
      liftIO $ TIO.writeFile debugPath rawMime
      liftIO $ TIO.putStrLn $ "Raw MIME dumped to " <> T.pack debugPath <> " (" <> show (T.length rawMime) <> " chars)"
  -- Now try the actual parse
  result <- fetchSettlementFileWithPlainPassword emailConfig (T.pack pass)
  case result of
    Left err -> do
      liftIO $ TIO.putStrLn $ "Parse error: " <> err
      liftIO exitFailure
    Right csvBytes -> do
      let csvFileName = extractAttachmentName debugResult
          csvPath = samplesDir </> csvFileName
          parsedPath = outDir </> csvFileName <> ".parsed.txt"
      -- Save downloaded CSV to samples/<pg>/
      liftIO $ LBS.writeFile csvPath csvBytes
      liftIO $ TIO.putStrLn $ "CSV saved to: " <> T.pack csvPath <> " (" <> show (LBS.length csvBytes) <> " bytes)"
      if LBS.length csvBytes == 0
        then liftIO $ TIO.putStrLn "WARNING: extracted CSV is 0 bytes — check debug-raw-mime.txt"
        else do
          -- Parse and save to output/<pg>/
          let parseResult = parsePaymentSettlementCsv pg csvBytes
          writeResult parsedPath (formatPaymentResult parseResult)

-- ---------------------------------------------------------------------------
-- SFTP fetch test (key-based auth, handles .csv.zip files)
-- ---------------------------------------------------------------------------

testSftpFetch :: SettlementService -> String -> String -> Int -> String -> Maybe String -> IO ()
testSftpFetch pg keyPath userAtHost port remotePath mbPattern = do
  let pgDir = pgDirName pg
      samplesDir = "samples" </> pgDir
      outDir = "output" </> pgDir
  liftIO $ createDirectoryIfMissing True samplesDir
  liftIO $ createDirectoryIfMissing True outDir

  -- Parse user@host into (username, host)
  let (sftpUser, sftpHost) = case break (== '@') userAtHost of
        (u, '@' : h) -> (T.pack u, T.pack h)
        _ -> (T.pack userAtHost, T.pack userAtHost)

  -- Build SFTPConfig that mirrors what production would use
  let sftpConfig =
        SFTPConfig
          { host = sftpHost,
            port = port,
            username = sftpUser,
            password = Encrypted "", -- not used for key-based auth
            remotePath = T.pack remotePath,
            privateKeyPath = Just (T.pack keyPath)
          }

  -- Step 1: List remote files (no list function in production, use sftp CLI)
  liftIO $ TIO.putStrLn $ "Connecting to SFTP server: " <> T.pack userAtHost <> " (port " <> show port <> ") ..."
  liftIO $ TIO.putStrLn $ "Remote path: " <> T.pack remotePath
  let sftpArgs = ["-i", keyPath, "-P", P.show port, "-o", "StrictHostKeyChecking=no", userAtHost]
  (listExit, listOut, listErr) <-
    liftIO $
      readProcessWithExitCode
        "sftp"
        sftpArgs
        ("cd " <> remotePath <> "\nls\nquit\n")
  case listExit of
    ExitFailure code -> do
      liftIO $ TIO.putStrLn $ "SFTP connection failed (exit " <> show code <> "): " <> T.pack listErr
      liftIO exitFailure
    ExitSuccess -> do
      let allFiles = extractSftpFileList listOut
          matchingFiles = case mbPattern of
            Nothing -> filter (\f -> ".csv" `isSuffixOf` f || ".csv.zip" `isSuffixOf` f) allFiles
            Just pat -> filter (\f -> pat `isInfixOf` f) allFiles
      liftIO $ TIO.putStrLn $ "Remote files found: " <> show (length allFiles)
      liftIO $ TIO.putStrLn $ "Matching files: " <> show (length matchingFiles)
      if null matchingFiles
        then do
          liftIO $ TIO.putStrLn "No matching files found. All remote files:"
          mapM_ (\f -> liftIO $ TIO.putStrLn $ "  " <> T.pack f) allFiles
          liftIO exitFailure
        else pure ()

      -- Step 2: Download each file using the real fetchSettlementFile
      forM_ matchingFiles $ \remoteFile -> do
        liftIO $ TIO.putStrLn $ "\nDownloading via fetchSettlementFile: " <> T.pack remoteFile
        dlResult <- SFTPSource.fetchSettlementFile sftpConfig (T.pack remoteFile)
        case dlResult of
          Left err -> do
            liftIO $ TIO.putStrLn $ "  Download failed: " <> err
          Right fileBytes -> do
            let localPath = samplesDir </> remoteFile
            liftIO $ LBS.writeFile localPath fileBytes
            liftIO $ TIO.putStrLn $ "  Saved to: " <> T.pack localPath <> " (" <> show (LBS.length fileBytes) <> " bytes)"

            -- Step 3: Unzip if needed, then parse
            csvFiles <-
              if ".zip" `isSuffixOf` remoteFile
                then do
                  liftIO $ TIO.putStrLn "  Unzipping..."
                  (uzExit, _uzOut, uzErr) <-
                    liftIO $
                      readProcessWithExitCode "unzip" ["-o", localPath, "-d", samplesDir] ""
                  case uzExit of
                    ExitFailure code -> do
                      liftIO $ TIO.putStrLn $ "  Unzip failed (exit " <> show code <> "): " <> T.pack uzErr
                      pure []
                    ExitSuccess -> do
                      let csvName = dropExtension remoteFile -- strip .zip -> .csv
                      let csvPath = samplesDir </> csvName
                      exists <- liftIO $ doesFileExist csvPath
                      if exists
                        then pure [csvPath]
                        else do
                          dirFiles <- liftIO $ listDirectory samplesDir
                          let newCsvs = filter (\f -> ".csv" `isSuffixOf` f && f /= remoteFile) dirFiles
                          pure $ map (samplesDir </>) newCsvs
                else pure [localPath]

            -- Step 4: Parse each CSV
            forM_ csvFiles $ \csvPath -> do
              liftIO $ TIO.putStrLn $ "  Parsing: " <> T.pack csvPath
              csvData <- liftIO $ LBS.readFile csvPath
              let parsedPath = outDir </> takeFileName csvPath <> ".parsed.txt"
                  parseResult = parsePaymentSettlementCsv pg csvData
              writeResult parsedPath (formatPaymentResult parseResult)

-- | Extract file names from SFTP 'ls' output
extractSftpFileList :: String -> [String]
extractSftpFileList output =
  let ls = lines output
      candidates = filter (not . null) $ map (last . words) $ filter (\l -> not ("sftp>" `isPrefixOf` l) && not (null l)) ls
   in filter (\f -> '.' `elem` f) candidates -- only keep entries with extensions

-- | Extract the attachment filename from raw MIME, fallback to "email-attachment.csv"
extractAttachmentName :: Maybe Text -> FilePath
extractAttachmentName Nothing = "email-attachment.csv"
extractAttachmentName (Just rawMime) =
  let lns = T.lines rawMime
      filenameLine = find (\l -> "filename=" `T.isInfixOf` T.toLower l) lns
   in case filenameLine of
        Just l ->
          let stripped = T.strip l
              -- Extract value after filename=, remove quotes
              afterEq = T.dropWhile (/= '=') stripped
              name = T.filter (/= '"') (T.drop 1 afterEq)
           in if T.null name then "email-attachment.csv" else T.unpack (T.strip name)
        Nothing -> "email-attachment.csv"

parsePG :: String -> IO SettlementService
parsePG s = case map Char.toLower s of
  "hyperpg" -> pure HyperPG
  "billdesk" -> pure BillDesk
  "yesbiz" -> pure YesBiz
  _ -> do
    liftIO $ TIO.putStrLn $ "Unknown PG: " <> T.pack s
    liftIO $ TIO.putStrLn "Supported PGs: hyperpg, billdesk, yesbiz"
    liftIO exitFailure

pgDirName :: SettlementService -> FilePath
pgDirName HyperPG = "hyperpg"
pgDirName BillDesk = "billdesk"
pgDirName YesBiz = "yesbiz"

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
  liftIO $ TIO.putStrLn "test-settlement-parser -- local CSV parsing & email fetch test tool"
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "Usage:"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- <pg> <payment|payout> <csv-file>"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- email <pg> <user> <password> [subject-filter]"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- sftp <pg> <key-path> <user@host> <remote-path> [port] [file-pattern]"
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
  liftIO $ TIO.putStrLn "Email fetch (IMAP → CSV attachment → parse):"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- email billdesk user@gmail.com app-password"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- email billdesk user@gmail.com app-password \"Settlement Report\""
  liftIO $ TIO.putStrLn ""
  liftIO $ TIO.putStrLn "SFTP fetch (key-based auth → download → unzip → parse):"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- sftp hyperpg ~/.ssh/my-key user@host /CUMTA/2026/01/01"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- sftp hyperpg ~/.ssh/my-key user@host /CUMTA/2026/01/01 22"
  liftIO $ TIO.putStrLn "  cabal run test-settlement-parser -- sftp hyperpg ~/.ssh/my-key user@host /CUMTA/2026/01/01 22 UPI_YP"
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
