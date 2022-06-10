{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Storage.Tabular.BecknRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Beckn.Types.BecknRequest as Domain
import Beckn.Types.Id

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
