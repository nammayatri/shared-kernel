{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Text where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude

compareWithoutRightSpaces :: Text -> Text -> Bool
compareWithoutRightSpaces = (==) `on` T.dropWhileEnd Char.isSpace

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

padLeft :: Int -> Char -> Text -> Text
padLeft n c txt = T.replicate (max 0 $ n - length txt) (T.singleton c) <> txt

-- Suits only for non-negative numbers
padNumber :: Integral i => Int -> i -> Text
padNumber n num = padLeft n '0' $ show (fromIntegral num :: Natural)

recursiveStrip :: String -> String
recursiveStrip = \case
  ('_' : xs) -> recursiveStrip xs
  a -> a

maskText :: Text -> Text
maskText text =
  if length text > 6
    then T.take 3 text <> "..." <> T.takeEnd 3 text
    else "..."

truncateText :: Text -> Text
truncateText text =
  if length text > 100000 -- TODO: Change this to mutually decided value
    then T.take 100 text <> "..."
    else text

camelCaseToSnakeCase :: Text -> Text
camelCaseToSnakeCase =
  T.concatMap \c ->
    if Char.isUpper c then T.pack ['_', Char.toLower c] else T.singleton c

validateAllDigitWithMinLength :: Int -> Text -> Bool
validateAllDigitWithMinLength minLength str = validateLength && validateAllDigits (T.unpack str)
  where
    validateLength = T.length str == minLength
    validateAllDigits (x : xs) = fromEnum x <= 57 && fromEnum x >= 48 && validateAllDigits xs
    validateAllDigits [] = True
