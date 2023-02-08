module Kernel.Storage.Queries.BecknRequest where

import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Tabular.BecknRequest ()
import Kernel.Types.BecknRequest
import Kernel.Utils.Common

logBecknRequest :: Text -> Text -> SqlDB ()
logBecknRequest reqJSON sign = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create $
    BecknRequest
      { id = uuid,
        timeStamp = now,
        becknRequest = reqJSON,
        signatureHeader = sign
      }
