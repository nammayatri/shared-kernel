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
