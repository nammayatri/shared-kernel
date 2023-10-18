{-# LANGUAGE TemplateHaskell #-}

module Kernel.Beam.Lib.UtilsTH
  ( enableKVPG,
    HasSchemaName (..),
    mkTableInstances,
    mkTableInstancesWithTModifier,
    mkTableInstancesGenericSchema,
    mkTableInstancesGenericSchemaWithTModifier,
    mkBeamInstancesForEnum,
    mkBeamInstancesForList,
    mkBeamInstancesForJSON,
    mkCustomMappings,
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HMI
import qualified Data.HashMap.Strict as HM
import Data.List (init, lookup, (!!))
import Data.List.Extra (anySame)
import qualified Data.Map.Strict as M
import qualified Data.Serialize as Serialize
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Schema.Tables as B
import Database.PostgreSQL.Simple.FromField (FromField (fromField), ResultError (UnexpectedNull))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified EulerHS.KVConnector.Types as KV
import EulerHS.Prelude hiding (Type, words)
import Kernel.Types.Common ()
import Kernel.Types.FromField (fromFieldEnum, fromFieldJSON)
import Kernel.Utils.Text (encodeToText)
import Language.Haskell.TH
import qualified Sequelize as S
import Text.Casing (camel, quietSnake)
import Prelude (head)
import qualified Prelude as P

-- | WARNING! Instances should be defined in application itself to avoid overlapping
class HasSchemaName tn where
  schemaName :: Proxy tn -> Text

emptyTextHashMap :: HMI.HashMap Text Text
emptyTextHashMap = HMI.empty

emptyValueHashMap :: M.Map Text (A.Value -> A.Value)
emptyValueHashMap = M.empty

enableKVPG :: Name -> [Name] -> [[Name]] -> Q [Dec]
enableKVPG name pKeyN sKeysN = do
  [tModeMeshSig, tModeMeshDec] <- tableTModMeshD name
  [kvConnectorDec] <- kvConnectorInstancesD name pKeyN sKeysN
  [meshMetaDec] <- meshMetaInstancesDPG name
  pure [tModeMeshSig, tModeMeshDec, meshMetaDec, kvConnectorDec] -- ++ cerealDec
  -- DB.OrderReferenceT (B.FieldModification (B.TableField DB.OrderReferenceT)) add signature

tableTModMeshD :: Name -> Q [Dec]
tableTModMeshD name = do
  let tableTModMeshN = mkName $ camel (nameBase name) <> "ModMesh"
  names <- extractRecFields . head . extractConstructors <$> reify name
  let recExps = (\name' -> (name', AppE (VarE 'B.fieldNamed) (LitE $ StringL $ nameBase name'))) <$> names
  return
    [ SigD tableTModMeshN (AppT (ConT name) (AppT (ConT ''B.FieldModification) (AppT (ConT ''B.TableField) (ConT name)))),
      FunD tableTModMeshN [Clause [] (NormalB (RecUpdE (VarE 'B.tableModification) recExps)) []]
    ]

------------------------------------------------------------------
-- class KVConnector table where
--   tableName :: Text
--   keyMap :: HM.HashMap Text Bool -- True implies it is primary key and False implies secondary
--   primaryKey :: table -> PrimaryKey
--   secondaryKeys:: table -> [SecondaryKey]
kvConnectorInstancesD :: Name -> [Name] -> [[Name]] -> Q [Dec]
kvConnectorInstancesD name pKeyN sKeysN = do
  let pKey = sortAndGetKey pKeyN
      sKeys = filter (\k -> pKey /= sortAndGetKey k) sKeysN
      pKeyPair = TupE [Just $ LitE $ StringL pKey, Just $ ConE 'True]
      sKeyPairs = map (\k -> TupE [Just $ LitE $ StringL $ sortAndGetKey k, Just $ ConE 'False]) sKeys
  let tableNameD = FunD 'KV.tableName [Clause [] (NormalB (LitE (StringL $ init $ camel (nameBase name)))) []]
      keyMapD = FunD 'KV.keyMap [Clause [] (NormalB (AppE (VarE 'HM.fromList) (ListE (pKeyPair : sKeyPairs)))) []]
      primaryKeyD = FunD 'KV.primaryKey [Clause [] (NormalB getPrimaryKeyE) []]
      secondaryKeysD = FunD 'KV.secondaryKeys [Clause [] (NormalB getSecondaryKeysE) []]
  return [InstanceD Nothing [] (AppT (ConT ''KV.KVConnector) (AppT (ConT name) (ConT $ mkName "Identity"))) [tableNameD, keyMapD, primaryKeyD, secondaryKeysD]]
  where
    getPrimaryKeyE =
      let obj = mkName "obj"
       in LamE [VarP obj] (AppE (ConE 'KV.PKey) (ListE (map (\n -> TupE [Just $ keyNameTextE n, Just $ getRecFieldE n obj]) pKeyN)))
    getSecondaryKeysE =
      if null sKeysN || sKeysN == [[]]
        then LamE [WildP] (ListE [])
        else do
          let obj = mkName "obj"
          LamE
            [VarP obj]
            ( ListE $
                map
                  (AppE (ConE 'KV.SKey) . ListE . map (\n -> TupE [Just $ keyNameTextE n, Just $ getRecFieldE n obj]))
                  sKeysN
            )
    getRecFieldE f obj =
      let fieldName = (splitColon $ nameBase f)
       in AppE (AppE (AppE (VarE 'utilTransform) (VarE 'emptyValueHashMap)) (LitE . StringL $ fieldName)) (AppE (VarE $ mkName fieldName) (VarE obj))
    keyNameTextE n = AppE (VarE 'T.pack) (LitE $ StringL (splitColon $ nameBase n))

sortAndGetKey :: [Name] -> String
sortAndGetKey names = do
  let sortedKeys = sort $ fmap (splitColon . nameBase) names
  intercalate "_" sortedKeys

--"$sel:merchantId:OrderReference" -> "merchantId"
splitColon :: String -> String
splitColon s = T.unpack $ T.splitOn ":" (T.pack s) !! 1

-------------------------------------------------------------------------------
meshMetaInstancesDPG :: Name -> Q [Dec]
meshMetaInstancesDPG name = do
  names <- extractRecFields . head . extractConstructors <$> reify name
  let modelTModMesh' = mkName $ camel (nameBase name) <> "ModMesh"
  let meshModelFieldModificationD = FunD 'KV.meshModelFieldModification [Clause [] (NormalB (VarE modelTModMesh')) []]
      valueMapperD = FunD 'KV.valueMapper [Clause [] (NormalB (VarE 'emptyValueHashMap)) []]
  let parseFieldAndGetClauseD = getParseFieldAndGetClauseD name names
      parseSetClauseD = getParseSetClauseD name names
  return [InstanceD Nothing [] (AppT (AppT (ConT ''KV.MeshMeta) (ConT ''Postgres)) (ConT name)) [meshModelFieldModificationD, valueMapperD, parseFieldAndGetClauseD, parseSetClauseD]]

--------------- parseFieldAndGetClause instance -------------------
getParseFieldAndGetClauseD :: Name -> [Name] -> Dec
getParseFieldAndGetClauseD name names = do
  let obj = mkName "obj"
      field = mkName "fieldName"
  let patternMatches = (\n -> Match (LitP $ StringL (nameBase n)) (NormalB (parseFieldAndGetClauseE name n)) []) <$> names
      failExp = AppE (VarE 'fail) (LitE $ StringL ("Where clause decoding failed for " <> nameBase name <> " - Unexpected column " <> nameBase field))
  let matchE = AppE (AppE (AppE (VarE 'HM.findWithDefault) (VarE field)) (VarE field)) (VarE 'emptyTextHashMap)
      caseExp = CaseE matchE (patternMatches ++ [Match WildP (NormalB failExp) []])
  FunD 'KV.parseFieldAndGetClause [Clause [VarP obj, VarP field] (NormalB caseExp) []]

parseFieldAndGetClauseE :: Name -> Name -> Exp
parseFieldAndGetClauseE name key = do
  let v = mkName "obj"
  let parseExp = AppE (AppE (VarE 'parseField) (LitE $ StringL $ nameBase name)) (AppE (AppE (modifyFieldToHS name) (LitE $ StringL $ nameBase key)) (VarE v))
  AppE (AppE (VarE '(<$>)) (AppE (ConE 'KV.TermWrap) (VarE key))) parseExp

--------------- parseSetClause instance -------------------
getParseSetClauseD :: Name -> [Name] -> Dec
getParseSetClauseD name names = do
  let --fnName = mkName "parseSetClause"
      obj = mkName "obj"
      field = mkName "fieldName"
      parseKeyAndValue = mkName "parseKeyAndValue"
      setClause = mkName "setClause"
  let patternMatches = (\n -> Match (LitP $ StringL $ nameBase n) (NormalB (parseSetClauseE name n)) []) <$> names
      failExp = AppE (VarE 'fail) (LitE $ StringL ("Set clause decoding failed for " <> nameBase name <> " - Unexpected column " <> nameBase field))
  let matchE = AppE (AppE (AppE (VarE 'HM.findWithDefault) (VarE field)) (VarE field)) (VarE 'emptyTextHashMap)
      caseExp = CaseE matchE (patternMatches ++ [Match WildP (NormalB failExp) []])
  FunD
    'KV.parseSetClause
    [ Clause
        [VarP setClause]
        (NormalB (AppE (AppE (VarE 'mapM) (VarE parseKeyAndValue)) (VarE setClause)))
        [FunD parseKeyAndValue [Clause [TupP [VarP field, VarP obj]] (NormalB caseExp) []]]
    ]

parseSetClauseE :: Name -> Name -> Exp
parseSetClauseE name key = do
  let v = mkName "obj"
  let parseExp = AppE (AppE (VarE 'parseField) (LitE $ StringL $ nameBase name)) (AppE (AppE (modifyFieldToHS name) (LitE $ StringL $ nameBase key)) (VarE v))
  AppE (AppE (VarE '(<$>)) (AppE (ConE 'S.Set) (VarE key))) parseExp

------------------- Utils ------------------------
parseField :: (FromJSON a, MonadFail f) => Text -> A.Value -> f a
parseField modelName fieldObj = case A.fromJSON fieldObj of
  A.Success res -> pure res
  _ -> fail $ T.unpack $ "Error while decoding - Unable to parse field for " <> modelName <> " model"

-- modifyFieldToHS k = M.findWithDefault P.id k txnDetailToHSModifiers
modifyFieldToHS :: Name -> Exp
modifyFieldToHS _ = do
  let key = mkName "keyToBeModified"
      val = mkName "val"
  LamE [VarP key, VarP val] (AppE (AppE (AppE (AppE (VarE 'M.findWithDefault) (VarE 'P.id)) (VarE key)) (VarE 'emptyValueHashMap)) (VarE val))

extractConstructors :: Info -> [Con]
extractConstructors (TyConI (DataD _ _ _ _ cons _)) = cons
extractConstructors (TyConI (NewtypeD _ _ _ _ cons _)) = [cons]
extractConstructors _ = []

extractRecFields :: Con -> [Name]
extractRecFields (RecC _ bangs) = handleVarBang <$> bangs where handleVarBang (a, _, _) = a
extractRecFields _ = []

utilTransform :: (ToJSON a) => Map Text (A.Value -> A.Value) -> Text -> a -> Text
utilTransform modifyMap field value = do
  let res = case M.lookup field modifyMap of
        Just fn -> fn . A.toJSON $ value
        Nothing -> A.toJSON value
  case res of
    A.String r -> r
    A.Number n -> T.pack $ show n
    A.Bool b -> T.pack $ show b
    A.Array l -> T.pack $ show l
    A.Object o -> T.pack $ show o
    A.Null -> T.pack ""

mkEmod :: Name -> String -> Maybe String -> Q (Dec, Dec)
mkEmod name table mbSchema = do
  let schemaNameExp = mkSchemaNameExp name mbSchema
      schemaNameContext = mkSchemaNameContext name mbSchema
  let fnName = mkName $ (T.unpack . T.dropEnd 1 . T.pack $ camel (nameBase name)) <> "Table"
      tableTModN = mkName $ camel (nameBase name) <> "Mod"
      bodyExpr =
        InfixE
          (Just (AppE (VarE 'B.setEntitySchema) (AppE (ConE 'Just) schemaNameExp)))
          (VarE '(<>))
          ( Just
              ( InfixE
                  (Just (AppE (VarE 'B.setEntityName) (LitE $ StringL table)))
                  (VarE '(<>))
                  ( Just
                      ( AppE
                          (VarE 'B.modifyTableFields)
                          (VarE tableTModN)
                      )
                  )
              )
          )
  let beTypeVar = VarT $ mkName "be"
      dbTypeVar = VarT $ mkName "dn"
      fnTypeBody =
        ForallT [] schemaNameContext $
          ConT ''B.EntityModification
            `AppT` (ConT ''B.DatabaseEntity `AppT` beTypeVar `AppT` dbTypeVar)
            `AppT` beTypeVar
            `AppT` (ConT ''B.TableEntity `AppT` ConT name)
      fnSig = SigD fnName fnTypeBody
      fnBody = FunD fnName [Clause [] (NormalB bodyExpr) []]
  return (fnSig, fnBody)

mkSchemaNameExp :: Name -> Maybe String -> Exp
mkSchemaNameExp name = \case
  Nothing -> VarE 'schemaName `AppE` (ConE 'Proxy `AppTypeE` ConT name)
  Just schema -> LitE $ StringL schema

mkSchemaNameContext :: Name -> Maybe String -> Cxt
mkSchemaNameContext name = \case
  Nothing -> [ConT ''HasSchemaName `AppT` ConT name]
  Just _ -> []

mkModelMetaInstances :: Name -> String -> Maybe String -> Q Dec
mkModelMetaInstances name table mbSchema = do
  let tableTModN = mkName $ camel (nameBase name) <> "Mod"
      modelFMInstance = FunD 'S.modelFieldModification [Clause [] (NormalB (VarE tableTModN)) []]
      modelNameInstance = FunD 'S.modelTableName [Clause [] (NormalB (LitE $ StringL table)) []]
      schemaNameExp = mkSchemaNameExp name mbSchema
      schemaNameContext = mkSchemaNameContext name mbSchema
      modelSchemaInstance = FunD 'S.modelSchemaName [Clause [] (NormalB (AppE (ConE 'Just) schemaNameExp)) []]
  return $ InstanceD Nothing schemaNameContext (AppT (ConT ''S.ModelMeta) (ConT name)) [modelFMInstance, modelNameInstance, modelSchemaInstance]

mkTModFunction :: [(String, String)] -> Name -> Q (Dec, Dec)
mkTModFunction tableFieldModifier name = do
  let nameStr = nameBase name
  mbTypeName <- lookupTypeName nameStr
  typeName <- case mbTypeName of
    Nothing -> fail $ nameStr <> " should be type name"
    Just n -> pure n
  tableTypeInfo <- reify typeName
  fieldNames <- case tableTypeInfo of
    TyConI dec -> do
      case dec of
        DataD _ _ _ _ [constructor] _ -> do
          case constructor of
            RecC _ records -> pure $ records <&> (\(fieldName, _, _) -> fieldName)
            _ -> fail $ nameStr <> " should contain records"
        _ -> fail $ nameStr <> " should be data type with one constructor"
    _ -> fail $ nameStr <> " should be type name"
  let fieldNamesStr = nameBase <$> fieldNames
  checkFieldModifier tableFieldModifier fieldNamesStr
  let fieldExpressions =
        fieldNames <&> \fieldName -> do
          let fieldNameStr = nameBase fieldName
          let modifiedField = fromMaybe (quietSnake fieldNameStr) (lookup fieldNameStr tableFieldModifier)
          (fieldName, VarE 'B.fieldNamed `AppE` LitE (StringL modifiedField))
  let bodyExpr = RecUpdE (VarE 'B.tableModification) fieldExpressions
      tableTModN = mkName $ camel (nameBase name) <> "Mod"
      fnTypeBody =
        ConT name
          `AppT` (ConT ''B.FieldModification `AppT` (ConT ''B.TableField `AppT` ConT name))
      fnSig = SigD tableTModN fnTypeBody
      fnBody = FunD tableTModN [Clause [] (NormalB bodyExpr) []]
  return (fnSig, fnBody)

checkFieldModifier :: [(String, String)] -> [String] -> Q ()
checkFieldModifier modifier fields = do
  when (anySame (fst <$> modifier)) $
    fail "Duplicated fields in modifier"
  when (anySame (snd <$> modifier)) $
    fail "Duplicated fields after modifying"
  forM_ modifier $ \(fieldBefore, _fieldAfter) -> do
    unless (fieldBefore `elem` fields) $
      fail ("Field: " <> fieldBefore <> " does not exist")

mkSerialInstances :: Name -> Q Dec
mkSerialInstances name = do
  let putInstance = FunD 'Serialize.put [Clause [] (NormalB (AppE (VarE 'error) (LitE $ StringL ""))) []]
      getInstance = FunD 'Serialize.get [Clause [] (NormalB (AppE (VarE 'error) (LitE $ StringL ""))) []]
  return $ InstanceD Nothing [] (AppT (ConT ''Serialize.Serialize) (AppT (ConT name) (ConT $ mkName "Identity"))) [putInstance, getInstance]

mkFromJSONInstance :: Name -> Q Dec
mkFromJSONInstance name = do
  let fromJSONFn = mkName "parseJSON"
      fromJSONInstance = FunD fromJSONFn [Clause [] (NormalB (AppE (VarE 'A.genericParseJSON) (VarE 'A.defaultOptions))) []]
  return $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (AppT (ConT name) (ConT $ mkName "Identity"))) [fromJSONInstance]

mkToJSONInstance :: Name -> Q Dec
mkToJSONInstance name = do
  let toJSONFn = mkName "toJSON"
      toJSONInstance = FunD toJSONFn [Clause [] (NormalB (AppE (VarE 'A.genericToJSON) (VarE 'A.defaultOptions))) []]
  return $ InstanceD Nothing [] (AppT (ConT ''ToJSON) (AppT (ConT name) (ConT $ mkName "Identity"))) [toJSONInstance]

mkShowInstance :: Name -> Q Dec
mkShowInstance name = do
  return $ StandaloneDerivD (Just StockStrategy) [] (AppT (ConT ''Show) (AppT (ConT name) (ConT $ mkName "Identity")))

mkTableInstances :: Name -> String -> String -> Q [Dec]
mkTableInstances name table schema = mkTableInstances' name table (Just schema) []

mkTableInstancesWithTModifier :: Name -> String -> String -> [(String, String)] -> Q [Dec]
mkTableInstancesWithTModifier name table schema = mkTableInstances' name table (Just schema)

mkTableInstancesGenericSchema :: Name -> String -> Q [Dec]
mkTableInstancesGenericSchema name table = mkTableInstances' name table Nothing []

mkTableInstancesGenericSchemaWithTModifier :: Name -> String -> [(String, String)] -> Q [Dec]
mkTableInstancesGenericSchemaWithTModifier name table = mkTableInstances' name table Nothing

mkTableInstances' :: Name -> String -> Maybe String -> [(String, String)] -> Q [Dec]
mkTableInstances' name table mbSchema tableFieldModifier = do
  modelMetaInstances <- mkModelMetaInstances name table mbSchema
  (eModSig, eModBody) <- mkEmod name table mbSchema
  serialInstances <- mkSerialInstances name
  fromJSONInstances <- mkFromJSONInstance name
  toJSONInstances <- mkToJSONInstance name
  customMappings <- mkCustomMappings name tableFieldModifier
  showInstances <- mkShowInstance name
  (tModSig, tModBody) <- mkTModFunction tableFieldModifier name
  pure ([tModSig, tModBody, modelMetaInstances, eModSig, eModBody, serialInstances, fromJSONInstances, toJSONInstances, showInstances] <> customMappings)

------------------- instances for table row ---------------

mkCustomMappings :: Name -> [(String, String)] -> Q [Dec]
mkCustomMappings name valuePairs = do
  let toJSONInstance = ValD (VarP 'KV.getTableMappings) (NormalB (ListE (map (\(key, value) -> TupE [Just (LitE (StringL key)), Just (LitE (StringL value))]) valuePairs))) []
  return [InstanceD Nothing [] (AppT (ConT ''KV.TableMappings) (AppT (ConT name) (ConT $ mkName "Identity"))) [toJSONInstance]]

-- | A set of instances required for beam table row as enum.
mkBeamInstancesForEnum :: Name -> Q [Dec]
mkBeamInstancesForEnum name = do
  let tyQ = pure (ConT name)
  [d|
    instance FromField $tyQ where
      fromField = fromFieldEnum

    instance HasSqlValueSyntax be String => HasSqlValueSyntax be $tyQ where
      sqlValueSyntax = autoSqlValueSyntax

    instance BeamSqlBackend be => B.HasSqlEqualityCheck be $tyQ

    instance FromBackendRow Postgres $tyQ
    |]

-- | A set of instances required for beam table row as list.
mkBeamInstancesForList :: Name -> Q [Dec]
mkBeamInstancesForList name = do
  let tyQ = pure (ConT name)
  [d|
    instance FromField [$tyQ] where
      fromField f mbValue = case mbValue of
        Nothing -> DPSF.returnError UnexpectedNull f mempty
        Just _ -> V.toList <$> fromField f mbValue

    instance FromField $tyQ where
      fromField = fromFieldEnum

    instance HasSqlValueSyntax be (V.Vector Text) => HasSqlValueSyntax be [$tyQ] where
      sqlValueSyntax x = sqlValueSyntax (V.fromList (T.pack . show <$> x))

    instance BeamSqlBackend be => B.HasSqlEqualityCheck be [$tyQ]

    instance FromBackendRow Postgres [$tyQ]
    |]

-- | A set of instances required for beam table row as json.
mkBeamInstancesForJSON :: Name -> Q [Dec]
mkBeamInstancesForJSON name = do
  let tyQ = pure (ConT name)
  [d|
    instance FromField $tyQ where
      fromField = fromFieldJSON

    instance HasSqlValueSyntax be Text => HasSqlValueSyntax be $tyQ where
      sqlValueSyntax = sqlValueSyntax . encodeToText

    instance BeamSqlBackend be => B.HasSqlEqualityCheck be $tyQ

    instance FromBackendRow Postgres $tyQ
    |]
