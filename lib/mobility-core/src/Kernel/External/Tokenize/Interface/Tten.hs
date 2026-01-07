module Kernel.External.Tokenize.Interface.Tten where

import qualified Data.Text as T
import Data.Time (localTimeToUTC, utc)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Tokenize.Interface.Error
import Kernel.External.Tokenize.Interface.Types as InterfaceTypes
import Kernel.External.Tokenize.Tten.Flow as TtenFlow
import Kernel.External.Tokenize.Tten.Types as TtenTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Error.Throwing (fromMaybeM)
import Kernel.Utils.Servant.Client

tokenize :: (CoreMetrics m, EncFlow m r, HasRequestId r, MonadReader r m) => TtenTypes.TtenTokenizationConfig -> m InterfaceTypes.TokenizationResp
tokenize config = do
  resp <- TtenFlow.generateToken config
  mkTokenizationResp resp
  where
    mkTokenizationResp TtenTypes.GenerateTokenResp {..} = do
      token <- fromMaybeM (TokenNotFound "TTEN") $ (.access_token) <$> data_
      let expiresAt = data_ >>= (parseTime . (.expires_at))
          scope = Nothing
      return $ InterfaceTypes.TokenizationResp {..}
      where
        parseTime :: Text -> Maybe UTCTime
        parseTime time = localTimeToUTC utc <$> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack time)
