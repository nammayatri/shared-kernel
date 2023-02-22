 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Logging where

import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, Show, Eq, Ord, FromDhall, ToJSON)

class Log m where
  logOutput :: LogLevel -> Text -> m ()
  withLogTag :: Text -> m a -> m a

data LoggerConfig = LoggerConfig
  { level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool,
    prettyPrinting :: Bool
  }
  deriving (Generic, FromDhall)

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig =
  LoggerConfig
    { level = DEBUG,
      logFilePath = "/tmp/default",
      logToFile = False,
      logToConsole = False,
      logRawSql = False,
      prettyPrinting = False
    }
