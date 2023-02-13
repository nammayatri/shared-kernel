module Kernel.InternalAPI.Auth.API where

import Kernel.Prelude
import Servant

type API =
  "internal"
    :> "auth"
    :> Capture "token" Token
    :> Get '[JSON] PersonId

type Token = Text

type PersonId = Text
