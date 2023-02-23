 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Kernel.Utils.Version (Reexport.versionToText, readVersion) where

import Data.Text
import Kernel.Prelude as Prelude
import Kernel.Types.Error
import Kernel.Types.Logging
import Kernel.Types.Version as Reexport
import Kernel.Utils.Common

readVersion :: (MonadThrow m, Log m) => Text -> m Version
readVersion versionText = do
  let listOfVersionParts = splitOn "." versionText
  when (Prelude.length listOfVersionParts /= 3) (throwError VersionUnexpectedVersion)
  let [majorString, minorString, maintenanceString] = unpack <$> listOfVersionParts
  major <- fromMaybeM VersionUnexpectedVersion (readMaybe majorString)
  minor <- fromMaybeM VersionUnexpectedVersion (readMaybe minorString)
  maintenance <- fromMaybeM VersionUnexpectedVersion (readMaybe maintenanceString)
  return Version {..}
