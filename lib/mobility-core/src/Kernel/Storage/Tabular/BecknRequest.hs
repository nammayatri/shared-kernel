{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Storage.Tabular.BecknRequest where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Types.BecknRequest as Domain
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BecknRequestT sql=beckn_request
      id Text
      becknRequest Text
      signatureHeader Text
      timeStamp UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BecknRequestT where
  type DomainKey BecknRequestT = Id Domain.BecknRequest
  fromKey (BecknRequestTKey _id) = Id _id
  toKey (Id id) = BecknRequestTKey id

instance TType BecknRequestT Domain.BecknRequest where
  fromTType BecknRequestT {..} = do
    return $
      Domain.BecknRequest
        { id = Id id,
          ..
        }
  toTType Domain.BecknRequest {..} =
    BecknRequestT
      { id = getId id,
        ..
      }
