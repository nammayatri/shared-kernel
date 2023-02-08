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
