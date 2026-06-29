{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.External.Settlement.Utils.ParserUtils
  ( nonEmpty',
    parseAmount,
    parseDateTime,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

nonEmpty' :: Text -> Maybe Text
nonEmpty' t
  | T.null (T.strip t) = Nothing
  | otherwise = Just (T.strip t)

parseAmount :: Text -> HighPrecMoney
parseAmount t =
  case readMaybe (T.unpack $ T.strip t) of
    Just v -> v
    Nothing -> 0

parseDateTime :: Text -> Maybe UTCTime
parseDateTime t
  | T.null (T.strip t) = Nothing
  | otherwise =
    parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack $ T.strip t)
      <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (T.unpack $ T.strip t)
