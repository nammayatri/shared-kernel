{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Mock.Utils (module Kernel.Mock.Utils, maybeToEither) where

import Data.Aeson hiding (Error)
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra
import qualified Data.List as List
import Data.String.Conversions
import qualified Data.Text as T
import Data.Time
import Kernel.Randomizer (getRandomInRange)
import Kernel.Types.Beckn.Error
import Universum

-- | Read formatted time.
-- Here %F means the same as %Y-%m-%d, and %R acts like %H:%M.
-- Example: readUTCTime "2021-12-01 18:00"
readUTCTime :: Text -> Maybe UTCTime
readUTCTime = parseTimeM True defaultTimeLocale "%F %R" . T.unpack

textToError :: Text -> Error
textToError desc =
  Error
    { _type = CORE_ERROR,
      code = "400",
      path = Nothing,
      message = Just desc
    }

generateOrderId :: (MonadIO m) => m Text
generateOrderId = show <$> getRandomInRange (1000000, 9999999 :: Int)

whenRight :: Applicative m => Either e a -> (a -> m ()) -> m ()
whenRight eith f = either (\_ -> pure ()) f eith

encodeJSON :: (ToJSON a) => a -> BSL.ByteString
encodeJSON = Ae.encode . toJSON

decodeJSON :: (FromJSON a) => BS.ByteString -> Maybe a
decodeJSON bs = Ae.decode (BSL.fromStrict bs) >>= Ae.parseMaybe parseJSON

decodingErrorMessage :: BS.ByteString -> Text
decodingErrorMessage bs = "failed to decode JSON: " <> cs bs

decodeEitherJSON :: (FromJSON a) => BS.ByteString -> Either Text a
decodeEitherJSON bs = do
  val <- maybeToEither (decodingErrorMessage bs) (Ae.decode (BSL.fromStrict bs))
  first T.pack $ Ae.parseEither parseJSON val

findAndDecode :: (FromJSON a) => BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Either Text a
findAndDecode key list = maybeToEither errMsg (List.lookup key list) >>= decodeEitherJSON
  where
    errMsg = "failed to find key: " <> cs key
