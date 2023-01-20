module Beckn.Utils.Common
  ( module Beckn.Utils.Common,
    module Common,
    callBecknAPI,
  )
where

import Beckn.Prelude as Common (everyPossibleVariant, foldWIndex, identity, show)
import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Common
import Beckn.Types.Error.BaseError.HTTPError as Common
import Beckn.Types.Field as Common
import Beckn.Types.Id (ShortId (ShortId))
import Beckn.Utils.Context as Common
import Beckn.Utils.Error as Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI)
import Beckn.Utils.Error.DB as Common
import Beckn.Utils.Logging as Common
import Beckn.Utils.Servant.Client as Common
import Beckn.Utils.Shutdown as Common (Shutdown)
import Beckn.Utils.Text as Common
import Beckn.Utils.Time as Common
import qualified Crypto.Number.Generate as Cryptonite
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Test.RandomStrings as RS

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

generateOTPCode :: MonadFlow m => m Text
generateOTPCode =
  liftIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

tailMaybe :: [a] -> Maybe a
tailMaybe arr = headMaybe (reverse arr)