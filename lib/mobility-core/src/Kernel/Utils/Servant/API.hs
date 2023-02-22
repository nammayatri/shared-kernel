 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE PolyKinds #-}

module Kernel.Utils.Servant.API
  ( type (:>|),
  )
where

import Data.Kind (Type)
import Servant

-- | This behaves similarly to ':>' from API point of view, but in implementation
-- it attaches a parameter to /each/ separate endpoint, not /all/ of them at once.
--
-- For instance:
-- @
-- type API = Header "auth" Text :> (Get '[JSON] Text :<|> Post '[JSON] ())
-- @
--
-- requires the following implementation:
--
-- @
-- handlers :: Server API
-- handlers = \auth -> get auth :<|> new auth
-- @
--
-- But when ':>' is replaced with ':>|', you can write just
--
-- @
-- handlers = get :<|> auth
-- @
--
-- Note that ':>|' has fewer priority that ':<|>' so you can omit parentheses.
--
-- This operator is experimental, if you find ':>' more appropriate then use it.
type family (:>|) (pre :: k) (api :: Type) where
  pre :>| (api1 :<|> api2) = (pre :>| api1) :<|> (pre :>| api2)
  pre :>| api = pre :> api

infixr 2 :>|
