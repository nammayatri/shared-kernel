 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Types.App
  ( module Kernel.Types.App,
    Servant.BaseUrl,
    Aeson.Value,
  )
where

import Control.Lens.Operators
import Data.Aeson as Aeson (Value)
import Data.OpenApi
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Kernel.Types.Field (HasFields)
import Kernel.Types.Forkable
import Kernel.Types.Logging
import Kernel.Types.MonadGuid
import Kernel.Types.Time
import Servant
import qualified Servant.Client.Core as Servant

data EnvR r = EnvR
  { flowRuntime :: R.FlowRuntime,
    appEnv :: r
  }
  deriving (Generic)

type MonadFlow m =
  ( Monad m,
    MonadIO m,
    L.MonadFlow m,
    Forkable m,
    Log m,
    MonadGuid m,
    MonadTime m,
    MonadClock m,
    MonadThrow m
  )

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( MonadFlow m,
    MonadReader r m,
    HasFields r fields
  )

type FlowHandlerR r = ReaderT (EnvR r) IO

type FlowServerR r api = ServerT api (FlowHandlerR r)

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

type Limit = Int

type Offset = Int

type RegToken = Text

-- FIXME: remove this
type AuthHeader = Header' '[Required, Strict] "token" RegToken

instance ToSchema Servant.BaseUrl where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "BaseUrl") aSchema

instance ToSchema Value where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "Value") $
        aSchema
          & description
            ?~ "Some JSON."
