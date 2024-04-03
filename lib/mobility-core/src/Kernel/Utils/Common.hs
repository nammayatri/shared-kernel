{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Common
  ( module Kernel.Utils.Common,
    module Common,
    callBecknAPI,
  )
where

import qualified Crypto.Number.Generate as Cryptonite
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import Kernel.Prelude as Common (everyPossibleVariant, foldWIndex, identity, show)
import Kernel.Types.Beckn.Ack as Common
import Kernel.Types.CacheFlow as Common
import Kernel.Types.Common as Common
import Kernel.Types.Error.BaseError.HTTPError as Common
import Kernel.Types.Field as Common
import Kernel.Types.Id (ShortId (ShortId))
import Kernel.Utils.Context as Common
import Kernel.Utils.Error as Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI)
import Kernel.Utils.Error.DB as Common
import Kernel.Utils.Logging as Common
import Kernel.Utils.Servant.Client as Common
import Kernel.Utils.Shutdown as Common (Shutdown)
import Kernel.Utils.Text as Common
import Kernel.Utils.Time as Common
import qualified Test.RandomStrings as RS

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

generateOTPCode :: MonadFlow m => m Text
generateOTPCode =
  liftIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

generateAplhaNumbericCode :: MonadFlow m => Int -> m Text
generateAplhaNumbericCode len = T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) len)
