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

import Data.Text hiding (reverse)
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
    _ : deviceModel : version' : xs -> do
      let mbManufacturer = getManufacturer xs
      let [deviceTypeStr, deviceVersion] = T.splitOn " v" version'
      deviceType <- case T.toLower deviceTypeStr of
        "android" -> Just ANDROID
        "ios" -> Just IOS
        _ -> Nothing
      return Device {deviceType = deviceType, deviceVersion = deviceVersion, deviceModel = deviceModel, deviceManufacturer = mbManufacturer}
    _ -> Nothing

mkClientDevice :: Maybe DeviceType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Device
mkClientDevice clientOsType clientOsVersion clientModel clientManufacturer =
  Device <$> clientOsType <*> clientOsVersion <*> clientModel <*> pure clientManufacturer

getManufacturer :: [Text] -> Maybe Text
getManufacturer xs = do
  manufacturer' <- listToMaybe (reverse xs)
  let parts = T.splitOn ":" manufacturer' -- expecting manufacturer at the end of deviceText
  case parts of
    ("mf" : manufacturer : _) -> Just manufacturer
    _ -> Nothing
