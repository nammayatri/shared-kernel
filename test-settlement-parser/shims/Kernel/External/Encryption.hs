{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kernel.External.Encryption
  ( EncKind (..),
    Encrypted (..),
    EncryptedField,
    EncFlow,
    decrypt,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Prelude (Eq, Show, pure, (.))

data EncKind
  = AsEncrypted
  | AsUnencrypted

newtype Encrypted a = Encrypted {unEncrypted :: a}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

type family EncryptedField (e :: EncKind) (a :: *) :: * where
  EncryptedField 'AsUnencrypted a = a
  EncryptedField 'AsEncrypted a = Encrypted a

-- | Shim: no real encryption, just unwrap the newtype
type EncFlow m r = (MonadIO m)

decrypt :: (MonadIO m) => Encrypted a -> m a
decrypt = pure . unEncrypted
