{-# LANGUAGE PackageImports #-}

module SettlementMimeCsv (extractMessageIds, extractCsvFromMime) where

import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude

extractMessageIds :: Text -> [Text]
extractMessageIds raw =
  let parts = T.words raw
   in reverse $ filter (T.all (\c -> c >= '0' && c <= '9')) parts

extractCsvFromMime :: Text -> Either Text LBS.ByteString
extractCsvFromMime mimeText =
  let linesList = map (T.dropWhileEnd (== '\r')) (T.lines mimeText)
      csvBoundaries = findCsvSection linesList
   in case csvBoundaries of
        Nothing -> Left "No CSV attachment found in email"
        Just content -> decodeContent content

findCsvSection :: [Text] -> Maybe [Text]
findCsvSection [] = Nothing
findCsvSection (line : rest)
  | isCsvContentType line =
    let bodyLines = dropWhile (not . T.null) rest
     in Just $ takeWhile (not . isBoundaryLine) (drop 1 bodyLines)
  | otherwise = findCsvSection rest
  where
    isCsvContentType l =
      let lower = T.toLower l
       in "content-type:" `T.isInfixOf` lower && ("text/csv" `T.isInfixOf` lower || "application/csv" `T.isInfixOf` lower)
    isBoundaryLine l = "--" `T.isPrefixOf` l

decodeContent :: [Text] -> Either Text LBS.ByteString
decodeContent contentLines =
  let joined = T.unlines contentLines
      stripped = T.strip joined
   in case B64.decode (TE.encodeUtf8 $ T.filter (/= '\n') stripped) of
        Right decoded -> Right $ LBS.fromStrict decoded
        Left _ -> Right $ LBS.fromStrict $ TE.encodeUtf8 stripped
