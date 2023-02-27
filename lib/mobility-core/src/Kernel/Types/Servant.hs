{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Servant where

import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import qualified Network.HTTP.Media as M
import Servant

data PlainText_ISO_8859_1 deriving (Typeable)

instance Accept PlainText_ISO_8859_1 where
  contentType _ = "text" M.// "plain" M./: ("charset", "ISO-8859-1")
