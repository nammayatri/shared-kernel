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
