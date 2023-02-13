module Kernel.Types.Credentials where

import Kernel.Prelude
import Kernel.Types.Base64
import Kernel.Types.Registry.Domain
import Kernel.Types.Registry.Subscriber (SubscriberType)
import Kernel.Utils.Dhall

type PrivateKey = Base64

type PublicKey = Base64

data Credential = Credential
  { shortOrgId :: Text,
    uniqueKeyId :: Text,
    signPubKey :: PublicKey,
    url :: BaseUrl,
    domain :: Domain,
    _type :: SubscriberType
  }
  deriving (Generic, FromDhall)
