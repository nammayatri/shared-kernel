module Kernel.External.SMS.KarixSms.Api where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import Kernel.External.SMS.KarixSms.Types
import qualified Network.HTTP.Media as M
import Servant

-- Custom MIME type that accepts text/json;charset=ISO-8859-1 (used by KarixSms API)
data KarixSmsJSON deriving (Typeable)

instance Accept KarixSmsJSON where
  contentTypes _ =
    "text" M.// "json" M./: ("charset", "ISO-8859-1")
      NE.:| ["application" M.// "json"]

instance MimeUnrender KarixSmsJSON KarixSmsSubmitRes where
  mimeUnrender _ = eitherDecode

instance MimeRender KarixSmsJSON KarixSmsRequest where
  mimeRender _ = encode

-- KarixSms JSON API: POST /httpapi/JsonReceiver (JSON body)
type ServiceAPI =
  "httpapi"
    :> "JsonReceiver"
    :> ReqBody '[JSON] KarixSmsRequest
    :> Post '[KarixSmsJSON] KarixSmsSubmitRes

karixSmsConnectAPI :: Proxy ServiceAPI
karixSmsConnectAPI = Proxy
