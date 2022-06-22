module Beckn.Storage.Queries.BecknRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Tabular.BecknRequest ()
import Beckn.Types.BecknRequest
import Beckn.Utils.Common

logBecknRequest :: Text -> Text -> SqlDB ()
logBecknRequest reqJSON sign = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create' $
    BecknRequest
      { id = uuid,
        timeStamp = now,
        becknRequest = reqJSON,
        signatureHeader = sign
      }
