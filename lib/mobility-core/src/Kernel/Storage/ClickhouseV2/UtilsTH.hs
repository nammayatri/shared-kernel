{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Kernel.Storage.ClickhouseV2.UtilsTH (mkClickhouseInstances, mkClickhouseInstancesDebug) where

import qualified Data.Aeson as A
import Data.Char (toLower)
import Data.List (head, tail)
import EulerHS.Prelude hiding (Type, words)
import qualified Kernel.Storage.ClickhouseV2.ClickhouseTable as CH
import qualified Kernel.Storage.ClickhouseV2.ClickhouseValue as CH
import Kernel.Types.Common ()
import Kernel.Utils.TH (mkTestSplice)
import Language.Haskell.TH

mkClickhouseInstances :: Name -> Name -> Q [Dec]
mkClickhouseInstances name selectModifier = do
  fields <- reifyFields name
  let fieldNames = fst <$> fields
  fromJSONValue <- mkFromJSONValue name fieldNames
  fromJSON <- mkFromJSON name fieldNames
  isClickhouseTableInstance <- mkClickhouseTableInstance name fields selectModifier
  pure [fromJSONValue, fromJSON, isClickhouseTableInstance]

-- | Will generate string for simple testing in repl. Using: putStrLn testSplice
mkClickhouseInstancesDebug :: Name -> Name -> Q [Dec]
mkClickhouseInstancesDebug name selectModifier = do
  decs <- mkClickhouseInstances name selectModifier
  testFunc <- mkTestSplice decs
  pure $ decs <> testFunc

reifyFields :: Name -> Q [(Name, Type)]
reifyFields name = do
  let nameStr = nameBase name
  mbTypeName <- lookupTypeName nameStr
  typeName <- case mbTypeName of
    Nothing -> fail $ nameStr <> " should be type name"
    Just n -> pure n
  tableTypeInfo <- reify typeName
  case tableTypeInfo of
    TyConI dec -> do
      case dec of
        DataD _ _ _ _ [constructor] _ -> do
          case constructor of
            RecC _ records -> forM records $ \(fieldName, _, fieldType) -> do
              case fieldType of
                AppT (AppT _c _f) a -> pure (fieldName, a)
                _ -> fail "field should have type C f a"
            _ -> fail $ nameStr <> " should contain records"
        _ -> fail $ nameStr <> " should be data type with one constructor"
    _ -> fail $ nameStr <> " should be type name"

mkClickhouseTableInstance :: Name -> [(Name, Type)] -> Name -> Q Dec
mkClickhouseTableInstance name fieldNames selectModifier = do
  let camelName = toLower (head $ nameBase name) : tail (nameBase name)
      fnN = mkName $ camelName <> "Table"
      fN = mkName "f"
      tN = mkName "t"
      fieldExpressions =
        fieldNames <&> \(fieldName, fieldType) -> do
          (fieldName, VarE fN `AppTypeE` fieldType `AppE` (VarE fieldName `AppE` VarE tN))
      nameC = mkName $ nameBase name
      bodyExpr = RecConE nameC fieldExpressions
      tableModificationFunc = FunD 'CH.tableModification [Clause [] (NormalB $ VarE fnN) []]
      mapTableFunc = FunD 'CH.mapTable [Clause [VarP fN, VarP tN] (NormalB bodyExpr) []]
      selectModifierFunc = FunD 'CH.getSelectModifier [Clause [WildP] (NormalB $ ConE selectModifier) []]
  return $ InstanceD Nothing [] (ConT ''CH.ClickhouseTable `AppT` ConT name) [tableModificationFunc, mapTableFunc, selectModifierFunc]

-- SPLICE:
-- instance ClickhouseTable FooT where
--   tableModification = fooTTable
--   mapTable f t = FooT {
--       bar1 = f @Bar1Type (bar1 t),
--       bar2 = f @Bar2Type (bar2 t),
--       bar3 = f @Bar3Type (bar3 t)
--     }

mkfieldModifierFunction :: Name -> Name -> [Name] -> [Dec]
mkfieldModifierFunction fnN name fieldNames = do
  let camelName = toLower (head $ nameBase name) : tail (nameBase name)
      --     fnN = mkName $ camelName <> "FieldModifier"
      caseExpressions =
        fieldNames <&> \fieldName -> do
          let matchBody = NormalB (VarE 'CH.getFieldModification `AppE` (VarE fieldName `AppE` VarE (mkName $ camelName <> "Table")))
          Match (LitP . StringL $ nameBase fieldName) matchBody []
      defaultCase = Match (VarP $ mkName "a") (NormalB . VarE $ mkName "a") []
      fieldExpression =
        ('A.fieldLabelModifier, LamCaseE $ caseExpressions <> [defaultCase])
      bodyExpr = RecUpdE (VarE 'A.defaultOptions) [fieldExpression]
      fnTypeBody = ConT ''A.Options
      fnSig = SigD fnN fnTypeBody
      fnBody = FunD fnN [Clause [] (NormalB bodyExpr) []]
  [fnSig, fnBody]

mkFromJSON :: Name -> [Name] -> Q Dec
mkFromJSON name fieldNames = do
  let fnN = 'A.parseJSON
      valN = mkName "val"
      tN = mkName "t"
      nameE = LitE (StringL $ nameBase name)
      paseJSONStmt = BindS (VarP tN) $ VarE 'A.parseJSON `AppTypeE` (ConT name `AppT` ConT ''CH.Value) `AppE` VarE valN
      fieldBinds =
        fieldNames <&> \fieldName -> do
          (fieldName, mkName (nameBase fieldName <> "'"))
      fieldStmts =
        fieldBinds <&> \(fieldName, fieldBind) -> do
          BindS (VarP fieldBind) (VarE 'parseField `AppE` nameE `AppE` LitE (StringL $ nameBase fieldName) `AppE` (VarE fieldName `AppE` VarE tN))
      recStmt = NoBindS $ VarE 'pure `AppE` RecConE (mkName $ nameBase name) (fieldBinds <&> second VarE)
      bodyExpr = DoE Nothing $ paseJSONStmt : fieldStmts <> [recStmt]
      fromJSONInstance = FunD fnN [Clause [VarP valN] (NormalB bodyExpr) []]
  return $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (AppT (ConT name) (ConT ''Identity))) [fromJSONInstance]

parseField :: (MonadFail m, CH.ClickhouseValue a) => String -> String -> CH.Value a -> m a
parseField tableName fieldName val = do
  either (\err -> fail $ "Could not parse clickhouse field " <> tableName <> "." <> fieldName <> ": " <> err) pure $ CH.getExcept (CH.fromClickhouseValue val)

-- SPLICE
-- instance FromJSON (FooT Identity) where
--   parseJSON val = do
--     t <- parseJSON @(FooT CH.Value) val
--     bar1' <- parseField "FooT" "bar1" (bar1 t)
--     bar2' <- parseField "FooT" "bar2" (bar2 t)
--     bar3' <- parseField "FooT" "bar3" (bar3 t)
--     pure FooT
--       { bar1 = bar1',
--         bar2 = bar2',
--         bar3 = bar3'
--       }

mkFromJSONValue :: Name -> [Name] -> Q Dec
mkFromJSONValue name fieldNames = do
  let fieldModifierN = mkName "fieldModifier"
  let fnN = 'A.parseJSON
      fromJSONInstance = FunD fnN [Clause [] (NormalB (AppE (VarE 'A.genericParseJSON) (VarE fieldModifierN))) (mkfieldModifierFunction fieldModifierN name fieldNames)]
  return $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (AppT (ConT name) (ConT ''CH.Value))) [fromJSONInstance]

-- SPLICE:
-- instance FromJSON (FooT CH.Value)
--   where parseJSON = genericParseJSON fieldModifier
--           where fieldModifier :: A.Options
--                 fieldModifier = A.defaultOptions{A.fieldLabelModifier = \case
--                   "bar1" -> getFieldModification (bar1 driverEdaKafkaTTable)
--                   "bar2" -> getFieldModification (bar1 driverEdaKafkaTTable)
--                   "bar3" -> getFieldModification (bar1 driverEdaKafkaTTable)
--                   a -> a}
