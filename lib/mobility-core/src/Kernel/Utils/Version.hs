{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Kernel.Utils.Version (Reexport.versionToText, readVersion, getDeviceFromText, mkClientDevice) where

import Data.Text
import qualified Data.Text as T
import Kernel.Prelude as Prelude
import Kernel.Types.Logging
import Kernel.Types.Version as Reexport

readVersion :: (MonadThrow m, Log m) => Text -> m Version
readVersion versionText =
  case textToVersion versionText of
    Right version -> return version
    _ -> pure $ Version {major = 0, minor = 0, maintenance = 0, preRelease = Nothing, build = Nothing}

getDeviceFromText :: Maybe Text -> Maybe Device
getDeviceFromText deviceText = do
  text <- deviceText
  let deviceInfo = T.splitOn "/" text
  case deviceInfo of
    _ : _ : version' : _ -> do
      let [deviceTypeStr, deviceVersion] = T.splitOn " v" version'
      deviceType <- case deviceTypeStr of
        "Android" -> Just ANDROID
        "iOS" -> Just IOS
        _ -> Nothing
      return Device {deviceType = deviceType, deviceVersion = deviceVersion}
    _ -> Nothing

mkClientDevice :: Maybe DeviceType -> Maybe Text -> Maybe Device
mkClientDevice clientOsType clientOsVersion =
  ((,) <$> clientOsType <*> clientOsVersion)
    <&> \(clientOsType', clientOsVersion') ->
      Device {deviceType = clientOsType', deviceVersion = clientOsVersion'}
