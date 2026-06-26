{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Types.ActorInfo
  ( ActorType (..),
    ActorInfo (..),
    HasActorInfo,
    withActorInfo,
  )
where

import Data.Aeson
import GHC.Records.Extra (HasField)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Field
import Kernel.Utils.Servant.Client
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data ActorType = UNKNOWN | SYSTEM | JOB | PERSON | DASHBOARD_PERSON
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''ActorType)

-- | Actor information for state transitions
data ActorInfo = ActorInfo
  { actorType :: ActorType,
    actorId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type HasActorInfo m r =
  ( HasFlowEnv m r '["actorInfo" ::: ActorInfo],
    HasRequestId r
  )

withActorInfo :: HasActorInfo m r => ActorType -> Maybe Text -> m a -> m a
withActorInfo actorType actorId action = local (\r -> r{actorInfo = ActorInfo {actorType, actorId}}) action

$(mkBeamInstancesForEnum ''ActorType)
