module Kernel.Utils.Error.OpenApi where

import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict.InsOrd as HM
import qualified Data.IntMap as M
import Data.List.NonEmpty as NE
import qualified Data.OpenApi as O
import qualified Data.Text as T
import Data.Typeable (typeRep)
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant
import Servant.OpenApi

data Throws (es :: [Type])

instance HasServer api ctx => HasServer (Throws es :> api) ctx where
  type ServerT (Throws es :> api) m = ServerT api m
  route _ ctx subserver = route (Proxy @api) ctx subserver
  hoistServerWithContext _ ctxp hst serv = hoistServerWithContext (Proxy @api) ctxp hst serv

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (Throws es :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data ErrorSchema = ErrorSchema
  { errorInfo :: ErrorInfo,
    errorSchema :: O.Schema,
    errorReference :: O.Reference
  }

class
  ( Typeable e,
    IsBaseError (ErrorInfoType e),
    IsHTTPError (ErrorInfoType e),
    IsAPIError (ErrorInfoType e)
  ) =>
  HasErrorInfo (e :: Type)
  where
  type ErrorInfoType e
  mkErrExample :: Proxy e -> (ErrorInfoType e)

class HasErrorInfoList (es :: [Type]) where
  mkErrorInfoList :: Proxy es -> [ErrorInfo]

instance HasErrorInfoList '[] where
  mkErrorInfoList _ = []

instance (HasErrorInfo e, HasErrorInfoList es) => HasErrorInfoList (e : es) where
  mkErrorInfoList _ = mkErrorInfo (Proxy @e) : mkErrorInfoList (Proxy @es)

mkErrorInfo :: HasErrorInfo e => Proxy e -> ErrorInfo
mkErrorInfo p = do
  let errExample = mkErrExample p
  ErrorInfo
    { errorName = removeTSuffix . show $ typeRep p,
      httpCode = httpCodeToCode $ toHttpCode errExample,
      errorCode = toErrorCode errExample,
      errorMessage = toMessage errExample
    }
  where
    removeTSuffix :: Text -> Text
    removeTSuffix txt = case T.unsnoc txt of
      Just (txt', 'T') -> txt'
      _ -> txt

mkErrorInfoSchema :: ErrorInfo -> O.Schema
mkErrorInfoSchema errInfo =
  mempty @O.Schema
    & O.type_ L.?~ O.OpenApiObject
    & O.properties
      L..~ HM.fromList
        [ ("errorCode", O.Inline $ mempty & O.type_ L.?~ O.OpenApiString & O.enum_ L.?~ [A.String $ errInfo.errorCode]),
          ("errorMessage", O.Inline $ mempty & O.type_ L.?~ O.OpenApiString & O.example L.?~ (maybe A.Null A.String $ errInfo.errorMessage))
        ]
    & O.required L..~ ["errorCode", "errorMessage"]

data ErrorInfo = ErrorInfo
  { errorName :: Text,
    httpCode :: Int,
    errorCode :: Text,
    errorMessage :: Maybe Text
  }

instance (HasOpenApi api, HasErrorInfoList es) => HasOpenApi (Throws es :> api) where
  toOpenApi _ = do
    let errorInfoList = mkErrorInfoList (Proxy @es)
        apiOpenApi = toOpenApi (Proxy @api)
        errorSchemas =
          errorInfoList <&> \errorInfo -> do
            let errorSchema = mkErrorInfoSchema errorInfo
            ErrorSchema {errorInfo, errorSchema, errorReference = O.Reference {getReference = errorInfo.errorName}}
        errorSchemasMap = HM.fromList $ errorSchemas <&> (\es -> (O.getReference es.errorReference, es.errorSchema))
    case errorSchemas of
      [] -> apiOpenApi
      (e : es) -> do
        apiOpenApi
          & O.paths L.%~ (fmap (addErrorResponse (e NE.:| es)))
          & O.components . O.schemas L.%~ HM.union errorSchemasMap -- _componentsResponses ??

addErrorResponse :: NonEmpty ErrorSchema -> O.PathItem -> O.PathItem
addErrorResponse ess pathItem =
  pathItem
    & O.get L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.put L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.post L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.delete L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.options L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.head_ L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.patch L.%~ (fmap $ addErrorResponseToOperation ess)
    & O.trace L.%~ (fmap $ addErrorResponseToOperation ess)

addErrorResponseToOperation :: NonEmpty ErrorSchema -> O.Operation -> O.Operation
addErrorResponseToOperation ess operation = do
  let sortedErrors = sortErrors ess
  foldl (\op (code, references) -> op & O.responses . L.at code L.%~ addErrorReferencesToResponse references) operation (M.toList sortedErrors)

sortErrors :: NonEmpty ErrorSchema -> M.IntMap (NonEmpty O.Reference)
sortErrors = foldl foldFunc M.empty
  where
    foldFunc :: M.IntMap (NonEmpty O.Reference) -> ErrorSchema -> M.IntMap (NonEmpty O.Reference)
    foldFunc acc es = M.alter (\mbV -> Just $ maybe (es.errorReference NE.:| []) (es.errorReference NE.<|) mbV) es.errorInfo.httpCode acc

addErrorReferencesToResponse :: NonEmpty O.Reference -> Maybe (O.Referenced O.Response) -> Maybe (O.Referenced O.Response)
addErrorReferencesToResponse errorReferences mbResponse = do
  let mediaType = "application/json;charset=utf-8"
  case mbResponse of
    Nothing ->
      Just $
        O.Inline $
          mempty @O.Response & O.content . L.at mediaType L.%~ addErrorReferencesToMediaTypeObject errorReferences
    Just (O.Inline response) ->
      Just $
        O.Inline $
          response & O.content . L.at mediaType L.%~ addErrorReferencesToMediaTypeObject errorReferences
    Just (O.Ref response) -> Just (O.Ref response) -- TODO check that it did not appear anywhere

addErrorReferencesToMediaTypeObject :: NonEmpty O.Reference -> Maybe O.MediaTypeObject -> Maybe O.MediaTypeObject
addErrorReferencesToMediaTypeObject errorReferences mbMediaTypeObject =
  mbMediaTypeObject
    & fromMaybe (mempty @O.MediaTypeObject)
    & O.schema L.%~ addErrorReferencesToResponseSchema errorReferences
    & Just

addErrorReferencesToResponseSchema :: NonEmpty O.Reference -> Maybe (O.Referenced O.Schema) -> Maybe (O.Referenced O.Schema)
addErrorReferencesToResponseSchema (errorReference NE.:| []) Nothing = Just $ O.Ref errorReference
addErrorReferencesToResponseSchema errorReferences Nothing =
  Just $ O.Inline $ mempty @O.Schema & O.oneOf L.?~ (O.Ref <$> NE.toList errorReferences)
addErrorReferencesToResponseSchema errorReferences (Just responseSchema) =
  Just $ O.Inline $ mempty @O.Schema & O.oneOf L.?~ (responseSchema : (O.Ref <$> NE.toList errorReferences))
