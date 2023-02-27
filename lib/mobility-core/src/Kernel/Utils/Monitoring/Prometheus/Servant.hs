{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Kernel.Utils.Monitoring.Prometheus.Servant where

import Data.Proxy
import Data.Text as DT
import EulerHS.Prelude as E
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.Wai (Request (..))
import Servant

class SanitizedUrl a where
  getSanitizedUrl :: Proxy a -> Request -> Maybe Text

instance
  (SanitizedUrl (a :: Type), SanitizedUrl (b :: Type)) =>
  SanitizedUrl (a :<|> b)
  where
  getSanitizedUrl _ req =
    getSanitizedUrl (Proxy :: Proxy a) req
      <|> getSanitizedUrl (Proxy :: Proxy b) req

instance
  ( KnownSymbol (path :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (path :> subroute)
  where
  getSanitizedUrl _ req = do
    let path = pathInfo req
    if E.null path
      then Nothing
      else do
        let (x : xs) = path
            p = DT.pack $ symbolVal (Proxy :: Proxy path)
        if p == x
          then
            let maybeUrl = getSanitizedUrl (Proxy :: Proxy subroute) $ req {pathInfo = xs}
             in (\url -> Just (p <> "/" <> url)) =<< maybeUrl
          else Nothing

instance
  ( KnownSymbol (capture :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (Capture capture a :> subroute)
  where
  getSanitizedUrl _ req = do
    let path = pathInfo req
    if E.null path
      then Nothing
      else
        let (_ : xs) = path
            p = DT.pack $ ":" <> symbolVal (Proxy :: Proxy capture)
            maybeUrl = getSanitizedUrl (Proxy :: Proxy subroute) $ req {pathInfo = xs}
         in (\url -> Just (p <> "/" <> url)) =<< maybeUrl

instance
  ReflectMethod m =>
  SanitizedUrl (Verb (m :: StdMethod) code contentType a)
  where
  getSanitizedUrl _ req = do
    let p = pathInfo req
    if E.null p && requestMethod req == reflectMethod (Proxy :: Proxy m)
      then Just ""
      else Nothing

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
