{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Kernel.Types.TimeRFC339 (module Kernel.Types.TimeRFC339, module Reexport) where

import Control.Lens hiding (Context)
import Data.Aeson
import Data.OpenApi (NamedSchema (NamedSchema), ToSchema (..), declareSchema, description)
import Data.Time (UTCTime)
import Data.Time.Format
import EulerHS.Prelude
import Kernel.Types.Beckn.Domain as Reexport
import Kernel.Utils.GenericPretty

newtype UTCTimeRFC3339 = UTCTimeRFC3339 UTCTime
  deriving (Show, Eq, Ord, Read, Generic, FromJSON)

instance PrettyShow UTCTimeRFC3339 where
  prettyShow = prettyShow . Showable

instance ToSchema UTCTimeRFC3339 where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "UTCTimeRFC3339") $
        aSchema
          & description
            ?~ "UTCTimeRFC3339 is a representation \
               \of UTCTime in milliseconds instead of microseconds."

instance ToJSON UTCTimeRFC3339 where
  toJSON (UTCTimeRFC3339 time) = toJSON (convertRFCStringToUTC (convertTimeToRFC time))

convertTimeToRFC :: UTCTime -> String
convertTimeToRFC = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

convertRFCStringToUTC :: String -> UTCTime
convertRFCStringToUTC = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

convertRFC3339ToUTC :: UTCTimeRFC3339 -> UTCTime
convertRFC3339ToUTC (UTCTimeRFC3339 time) = time
