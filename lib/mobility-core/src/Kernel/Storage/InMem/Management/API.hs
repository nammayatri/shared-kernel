module Kernel.Storage.InMem.Management.API where

import Kernel.Storage.InMem.Management.Types
import Servant

type InMemManagementAPI =
  "inMem"
    :> ( "keys" :> Get '[JSON] InMemKeysResponse
           :<|> "get" :> ReqBody '[JSON] InMemGetRequest :> Post '[JSON] InMemGetResponse
           :<|> "refresh" :> ReqBody '[JSON] InMemRefreshRequest :> Post '[JSON] InMemRefreshResponse
           :<|> "serverInfo" :> Get '[JSON] InMemServerInfoResponse
       )
