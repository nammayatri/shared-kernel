{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Kernel.Utils.Monitoring.Prometheus.Servant where

import qualified Data.List as List
import Data.Proxy
import Data.Text as DT
import EulerHS.Prelude as E
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.Wai (Request (..))
import Servant

-- Helper function to normalize paths by removing trailing slashes
normalizedPathInfo :: Request -> [Text]
normalizedPathInfo = List.dropWhileEnd (== "") . pathInfo

class SanitizedUrl a where
  getSanitizedUrl :: Proxy a -> Maybe Request -> Maybe Text

instance
  (SanitizedUrl (a :: Type), SanitizedUrl (b :: Type)) =>
  SanitizedUrl (a :<|> b)
  where
  getSanitizedUrl _ mbReq =
    getSanitizedUrl (Proxy :: Proxy a) mbReq
      <|> getSanitizedUrl (Proxy :: Proxy b) mbReq

instance
  ( KnownSymbol (path :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (path :> subroute)
  where
  getSanitizedUrl _ mbReq = do
    let p = DT.pack $ symbolVal (Proxy :: Proxy path)
    case mbReq of
      Nothing -> do
        url <- getSanitizedUrl (Proxy :: Proxy subroute) Nothing
        pure (p <> "/" <> url)
      Just req -> case normalizedPathInfo req of
        (x : xs) | x == p -> do
          url <- getSanitizedUrl (Proxy :: Proxy subroute) (Just req {pathInfo = xs})
          pure (p <> "/" <> url)
        _ -> Nothing

instance
  ( KnownSymbol (capture :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (Capture capture a :> subroute)
  where
  getSanitizedUrl _ mbReq = do
    let p = DT.pack $ ":" <> symbolVal (Proxy :: Proxy capture)
    case mbReq of
      Nothing -> do
        url <- getSanitizedUrl (Proxy :: Proxy subroute) Nothing
        pure (p <> "/" <> url)
      Just req -> case normalizedPathInfo req of
        (_ : xs) -> do
          url <- getSanitizedUrl (Proxy :: Proxy subroute) (Just req {pathInfo = xs})
          pure (p <> "/" <> url)
        [] -> Nothing

instance
  ReflectMethod m =>
  SanitizedUrl (Verb (m :: StdMethod) code contentType a)
  where
  getSanitizedUrl _ Nothing = Just ""
  getSanitizedUrl _ (Just req) =
    case normalizedPathInfo req of
      [] -> Just ""
      _ -> Nothing

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Description (h :: Symbol) :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Summary (h :: Symbol) :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (QueryParams (h :: Symbol) a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Header h a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (BasicAuth h a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (ReqBody cts a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (QueryParam' modifier name t :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Header' '[Required, Strict] h v :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance SanitizedUrl Raw where
  getSanitizedUrl _ _ = Nothing

-- Note: Add instances for custom auth types as needed:
-- instance SanitizedUrl (subroute :: Type) => SanitizedUrl (TokenAuth :> subroute) where
--   getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
