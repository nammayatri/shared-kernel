{-# LANGUAGE FlexibleContexts #-}

module Kernel.Prelude (module E, module Kernel.Prelude) where

import Control.Monad.IO.Class as E (MonadIO, liftIO)
import Data.Aeson as E (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Bool as E (bool)
import Data.Foldable as E
import Data.Function as E hiding (id)
import Data.Functor as E
import Data.Functor.Identity as E
import Data.Maybe as E
import Data.Proxy as E (Proxy (..))
import Data.String as E (IsString (..))
import Data.Text as E (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time as E (LocalTime, TimeOfDay)
import Data.Time.Clock as E (NominalDiffTime, UTCTime)
import Data.Traversable as E
import GHC.Generics as E (Generic, Generic1)
import GHC.Int as E (Int64)
import GHC.Stack as E (HasCallStack)
import Kernel.Prelude.OrphanInstances ()
import Text.Read as E (readMaybe)
import Prelude as E hiding (error, id, log, print, putStr, putStrLn, show, undefined)
import qualified Prelude as P

show :: P.Show a => a -> Text
show = T.pack . P.show

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . TIO.putStrLn

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . TIO.putStr

print :: (MonadIO m, P.Show a) => a -> m ()
print = liftIO . P.print

identity :: p -> p
identity a = a
