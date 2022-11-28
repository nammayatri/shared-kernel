{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Utils.Version (Reexport.versionToText, readVersion) where

import Beckn.Prelude as Prelude
import Beckn.Types.Error
import Beckn.Types.Logging
import Beckn.Types.Version as Reexport
import Beckn.Utils.Common
import Data.Text

readVersion :: (MonadThrow m, Log m) => Text -> m Version
readVersion versionText = do
  let listOfVersionParts = splitOn "." versionText
  when (Prelude.length listOfVersionParts /= 3) (throwError VersionUnexpectedVersion)
  let [majorString, minorString, maintenanceString] = unpack <$> listOfVersionParts
  major <- fromMaybeM VersionUnexpectedVersion (readMaybe majorString)
  minor <- fromMaybeM VersionUnexpectedVersion (readMaybe minorString)
  maintenance <- fromMaybeM VersionUnexpectedVersion (readMaybe maintenanceString)
  return Version {..}