module Kernel.External.Verification.HyperVerge.Flow where

import qualified Control.Concurrent.MVar as CCMVar
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as BSL
import Kernel.External.Verification.HyperVerge.Error
import Kernel.External.Verification.HyperVerge.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Utils.Common hiding (callAPI)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types (hContentType)
import Servant.API
import Servant.Client
import Servant.Client.Core
import Servant.Multipart
import Servant.Multipart.Client ()
import qualified Servant.Types.SourceT as S

type FaceValidationAPI =
  Header "transactionId" Text
    :> Header "appId" Text
    :> Header "appKey" Text
    :> MultipartForm Tmp HyperVergeSelfieValidationReq
    :> "v1"
    :> "checkLiveness"
    :> Post '[JSON] HyperVergeSelfieValidationRes

api :: Proxy FaceValidationAPI
api = Proxy

clientFunction :: Maybe Text -> Maybe Text -> Maybe Text -> (BL.ByteString, HyperVergeSelfieValidationReq) -> ClientM HyperVergeSelfieValidationRes
clientFunction = client api

callAPI :: Maybe Text -> Maybe Text -> Maybe Text -> HyperVergeSelfieValidationReq -> ClientM HyperVergeSelfieValidationRes
callAPI transactionId appId appKey formData = clientFunction transactionId appId appKey ("xxxxxx", formData)

callHyperVergeFaceValidationAPI ::
  (MonadFlow m, CoreMetrics m) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  HyperVergeSelfieValidationReq ->
  m HyperVergeSelfieValidationRes
callHyperVergeFaceValidationAPI url transactionId appId appKey req = do
  manager <- liftIO $ Client.newManager tlsManagerSettings
  logDebug $ "The request is : " <> (show req) --HVTODO: Remove this
  (liftIO $ runClientM (callAPI (Just transactionId) (Just appId) (Just appKey) req) (ClientEnv manager url Nothing modifiedHVMakeClientRequest)) >>= checkHyperVergeError url

checkHyperVergeError :: (MonadThrow m, Log m) => BaseUrl -> Either ClientError HyperVergeSelfieValidationRes -> m HyperVergeSelfieValidationRes
checkHyperVergeError url resp = do
  fromEitherM (hyperVergeError url) resp >>= validateResponseStatus

hyperVergeError :: BaseUrl -> ClientError -> ExternalAPICallError
hyperVergeError = ExternalAPICallError (Just "HYPERVERGE_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m) => HyperVergeSelfieValidationRes -> m HyperVergeSelfieValidationRes
validateResponseStatus response
  | response.statusCode == 200 = pure response
  | otherwise = throwError $ HyperVergeCallError (show response.statusCode) (show response)

modifiedHVMakeClientRequest :: BaseUrl -> Request -> Client.Request
modifiedHVMakeClientRequest burl r =
  Client.defaultRequest
    { Client.method = requestMethod r,
      Client.host = fromString $ baseUrlHost burl,
      Client.port = baseUrlPort burl,
      Client.path =
        BSL.toStrict $
          fromString (baseUrlPath burl)
            <> toLazyByteString (requestPath r),
      Client.queryString = buildQueryString . toList $ requestQueryString r,
      Client.requestHeaders =
        maybeToList acceptHdr ++ maybeToList contentTypeHdr ++ headers,
      Client.requestBody = body,
      Client.secure = isSecure
    }
  where
    -- Content-Type and Accept are specified by requestBody and requestAccept
    headers =
      filter (\(h, _) -> h /= "Accept" && h /= "Content-Type") $
        toList $ requestHeaders r

    acceptHdr
      | null hs = Nothing
      | otherwise = Just ("Accept", renderHeader hs)
      where
        hs = toList $ requestAccept r

    convertBody bd = case bd of
      RequestBodyLBS body' -> Client.RequestBodyLBS body'
      RequestBodyBS body' -> Client.RequestBodyBS body'
      RequestBodySource sourceIO -> Client.RequestBodyStreamChunked givesPopper
        where
          givesPopper :: (IO BS.ByteString -> IO ()) -> IO ()
          givesPopper needsPopper = S.unSourceT sourceIO $ \step0 -> do
            ref <- CCMVar.newMVar step0

            -- Note sure we need locking, but it's feels safer.
            let popper :: IO BS.ByteString
                popper = CCMVar.modifyMVar ref nextBs

            needsPopper popper

          nextBs S.Stop = return (S.Stop, BS.empty)
          nextBs (S.Error err) = fail err
          nextBs (S.Skip s) = nextBs s
          nextBs (S.Effect ms) = ms >>= nextBs
          nextBs (S.Yield lbs s) = case BSL.toChunks lbs of
            [] -> nextBs s
            (x : xs)
              | BS.null x -> nextBs step'
              | otherwise -> return (step', x)
              where
                step' = S.Yield (BSL.fromChunks xs) s

    (body, contentTypeHdr) = case requestBody r of
      Nothing -> (Client.RequestBodyBS "", Nothing)
      Just (body', typ) -> (convertBody body', Just (hContentType, renderHeader typ))

    isSecure = case baseUrlScheme burl of
      Http -> False
      Https -> True

    -- Query string builder which does not do any encoding
    buildQueryString [] = mempty
    buildQueryString _ = ""
