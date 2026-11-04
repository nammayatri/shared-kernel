{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Prelude (module E, module Kernel.Prelude) where

import Control.Arrow as E
import qualified Control.Concurrent as Conc
import Control.Concurrent.STM.TMVar as E (TMVar)
import Control.Exception.Safe as E hiding (assert)
import Control.Monad.Reader as E
import Control.Monad.Trans.Maybe as E
import Data.Aeson as E (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Bool as E (bool)
import Data.Fixed
import Data.Foldable as E
import Data.Function as E hiding (id)
import Data.Functor as E
import Data.Functor.Identity as E
import Data.Kind as E (Type)
import Data.List.NonEmpty as E (NonEmpty (..))
import Data.Maybe as E
import Data.OpenApi as E (ToParamSchema, ToSchema)
import Data.Proxy as E (Proxy (..))
import Data.String as E (IsString (..))
import Data.Text as E (Text)
import qualified Data.Text as T
import Data.Time as E (DiffTime, LocalTime, TimeOfDay (..), secondsToDiffTime)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock as E (NominalDiffTime, UTCTime)
import Data.Traversable as E
import GHC.Generics as E (Generic, Generic1)
import GHC.Int as E (Int64)
import GHC.Records.Compat as E
import GHC.Stack as E (HasCallStack)
import Kernel.Prelude.OrphanInstances ()
import Safe as E
import Servant.Client as E (BaseUrl (..))
import qualified Servant.Client as Servant
import Text.Read as E (readMaybe)
import Universum.Debug as E
import Universum.Print as E
import Universum.String.Conversion as E hiding (readMaybe)
import Prelude as E hiding (error, id, log, print, putStr, putStrLn, show, undefined)

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl' (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

identity :: p -> p
identity a = a

everyPossibleVariant :: (Enum a, Bounded a) => [a]
everyPossibleVariant = [minBound .. maxBound]

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do
  b <- mb
  when b thing

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb = whenM (not <$> mb)

showBaseUrl :: BaseUrl -> Text
showBaseUrl = T.pack . Servant.showBaseUrl

parseBaseUrl :: MonadThrow m => Text -> m BaseUrl
parseBaseUrl = Servant.parseBaseUrl . T.unpack

whileM :: Monad m => m Bool -> m () -> m ()
whileM b f = whenM b $ f >> whileM b f

threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO . Conc.threadDelay

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

intToNominalDiffTime :: Int -> NominalDiffTime
intToNominalDiffTime = secondsToNominalDiffTime . MkFixed . (* 1000000000000) . toInteger

timeOfDayToDiffTime :: TimeOfDay -> DiffTime
timeOfDayToDiffTime (TimeOfDay h m s) = secondsToDiffTime $ fromIntegral (h * 3600 + m * 60 + floor s)

-- fixme: test this function
roundToPowerOfTen :: RealFrac a => Int -> a -> a
roundToPowerOfTen tenExp a = do
  let f = 10 ^^ negate tenExp
      lifted = a * f
      eps = 1e-8
      roundVia func = fromIntegral @Integer (func lifted) / f
  if abs (abs (lifted - fromIntegral @Integer (floor lifted)) - 0.5) < eps
    then if a >= 0 then roundVia ceiling else roundVia floor
    else roundVia round

roundToUnits :: (RealFrac a) => a -> a
roundToUnits = roundToPowerOfTen 0

roundToIntegral :: (RealFrac a, Integral b) => a -> b
roundToIntegral = round . roundToUnits

showRounded :: RealFrac a => a -> Text
showRounded = show @_ @Int . roundToIntegral

infixl 1 >>=/

(>>=/) :: Monad m => m a -> (a -> m b) -> m a
(>>=/) a b = do
  a' <- a
  _ <- b a'
  return a'

infixl 1 /=<<

(/=<<) :: Monad m => (a -> m b) -> m a -> m a
(/=<<) a b = b >>=/ a

infixl 1 >>>=

(>>>=) :: (Traversable n, Monad m, Monad n) => m (n a) -> (a -> m (n b)) -> m (n b)
(>>>=) a b = do
  a >>= \a1 -> sequenceA (a1 <&> b) <&> join

infixl 1 =<<<

(=<<<) :: (Traversable n, Monad m, Monad n) => (a -> m (n b)) -> m (n a) -> m (n b)
(=<<<) a b = b >>>= a

hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

(|<|>|) :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
(|<|>|) funcA funcB = do
  maybeA <- funcA
  case maybeA of
    Just _ -> return maybeA
    Nothing -> funcB

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs
