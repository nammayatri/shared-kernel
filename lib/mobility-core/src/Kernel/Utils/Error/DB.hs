 {-
  Copyright 2022-23, Juspay India Pvt Ltd
  
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
  
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is 
  
  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
  
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero 
  
  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Error.DB where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Error.Throwing

throwDBError :: (MonadThrow m, Log m) => ET.DBError -> m a
throwDBError (ET.DBError dbErrType msg) = throwError $
  case dbErrType of
    ET.UnexpectedResult -> SQLResultError msg
    ET.SQLError sqlErr -> SQLRequestError (show sqlErr) msg
    _ -> DBUnknownError msg

checkDBError :: (MonadThrow m, Log m) => ET.DBResult a -> m a
checkDBError = either throwDBError pure

checkDBErrorOrEmpty ::
  (MonadThrow m, Log m, IsBaseException b) =>
  ET.DBResult (Maybe a) ->
  b ->
  m a
checkDBErrorOrEmpty dbres domainErrOnEmpty =
  either throwDBError (fromMaybeM domainErrOnEmpty) dbres
