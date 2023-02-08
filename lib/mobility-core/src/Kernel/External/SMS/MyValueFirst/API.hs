module Kernel.External.SMS.MyValueFirst.API where

import Kernel.External.SMS.MyValueFirst.Types
import Kernel.Types.App
  ( MandatoryQueryParam,
  )
import Kernel.Types.Servant
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type ServiceAPI =
  "smpp"
    :> "sendsms"
    :> MandatoryQueryParam "username" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "from" Text
    :> MandatoryQueryParam "to" Text
    :> MandatoryQueryParam "text" Text
    :> Post '[PlainText_ISO_8859_1] SubmitSmsRes

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

submitSms :: SubmitSms -> ET.EulerClient SubmitSmsRes
submitSms SubmitSms {..} = ET.client serviceAPI username password from to text
