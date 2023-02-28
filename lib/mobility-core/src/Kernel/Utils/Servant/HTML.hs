{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Servant.HTML where

import qualified Data.ByteString.Lazy as BS
import EulerHS.Prelude
import qualified Network.HTTP.Media as M
import Servant

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = pure $ "text" M.// "html" M./: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = BS.fromStrict

instance MimeUnrender HTML ByteString where
  mimeUnrender _ = pure . BS.toStrict
