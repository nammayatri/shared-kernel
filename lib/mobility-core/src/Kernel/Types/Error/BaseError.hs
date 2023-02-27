{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Types.Error.BaseError where

import Control.Exception
import EulerHS.Prelude hiding (Show, pack, show)
import Prelude (Show (..))

type IsBaseException e = (IsBaseError e, Exception e)

class IsBaseError e where
  toMessage :: e -> Maybe Text
  toMessage _ = Nothing

data BaseException = forall e. IsBaseException e => BaseException e

instance Show BaseException where
  show (BaseException e) = show e

instance Exception BaseException
