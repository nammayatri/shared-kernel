module Kernel.Utils.Common
  ( module Kernel.Utils.Common,
    module Common,
    callBecknAPI,
  )
where

import Kernel.Prelude as Common (everyPossibleVariant, foldWIndex, identity, show)
import Kernel.Types.Common as Common
import Kernel.Types.Beckn.Ack as Common
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
import qualified Crypto.Number.Generate as Cryptonite
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Test.RandomStrings as RS

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

generateOTPCode :: MonadFlow m => m Text
generateOTPCode =
  liftIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999
