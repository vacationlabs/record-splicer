{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module RecordSplices where

import           Control.Monad
import qualified Data.List as DL
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

data SpliceArgs = SpliceArgs
     {
        sourcePrefix :: String
     ,  source :: Name
     ,  requiredFields :: [Name]
     ,  targetName :: String
     ,  targetPrefix :: String
     ,  generateClassyLenses :: Bool
     ,  deriveClasses :: [Name]
     }

createRecordSplice :: SpliceArgs -> Q [Dec]
createRecordSplice args = do
  info <- reify $ source args

  case info of
    TyConI (TySynD name tyVars ty) -> do
      let tName = mkName $ targetName args
          dName = mkName $ targetName args ++ "Delta"
          -- List of type names used in the type synonym, with the data type as the first parameter
          paramTypeNames = traceShow (({- map (mkName . nameBase) -}(requiredFields args))) getTypes ty

      -- The data type info of the type synonym
      dataInfo <- reify $ paramTypeNames !! 0

      -- The list of type variables corresponding to the required fields
      fieldTypeVars <- getFieldsTypeVar $ (requiredFields args)

      -- The type variables in the data type
      let (ctx, paramVars, kind, fields, classes) = case dataInfo of
                                                        TyConI (DataD ctx name tVars kind [RecC _ fields] classes)-> (ctx, tVars, kind, fields, classes)
          -- An associative list of the type variables in data types and the exact types
          -- in the type synonym
          assocListNamesTyVars =  getTypeVars paramVars (drop 1 paramTypeNames)
      -- TODO : Find a name for each Type Variable in the fieldtypevars list, a
      --        corresponding name from the assoc list.

          (targetFields, deltaFields) = generateTargetFields' args assocListNamesTyVars fields
          fieldTypes = traceShow (targetFields) map (getFieldType assocListNamesTyVars) fieldTypeVars

      return
        [
          DataD ctx tName [] kind [RecC tName targetFields] classes
        , DataD ctx dName [] kind [RecC dName deltaFields] classes
        ]

sourceToTargetName :: SpliceArgs -> Name -> Name
sourceToTargetName SpliceArgs{..} name =
  mkName $ targetPrefix ++ (DL.drop (DL.length sourcePrefix) (nameBase name))

generateTargetFields :: SpliceArgs -> [Name] -> [Type] -> [VarBangType]
generateTargetFields _ [] _ = []
generateTargetFields args ((n:ns)) (t:ts) = ((sourceToTargetName args n), bang, t): (generateTargetFields args ns ts)
                     where
                       bang = Bang NoSourceUnpackedness NoSourceStrictness

getTypes :: Type -> [Name]
-- getTypes (AppT (ConT cons) (ConT var)) = [var]
getTypes (AppT x y) = getTypes x ++ getTypes y
getTypes (ConT name) = [name]

getFieldType :: [(TyVarBndr, Name)] -> Type -> Type
getFieldType (((KindedTV n1 kind), name) : xs) t@(VarT n2) | n1 == n2 = ConT name
                                                           | otherwise = getFieldType xs t
getFieldType _ t@(ConT n2) = t

getTypeVars :: [TyVarBndr] -> [Name] -> [(TyVarBndr, Name)]
getTypeVars = zip

getFieldTypeVar :: Name -> Q Type
getFieldTypeVar name = do
  info <- reify name
  case info of
    VarI _ (ForallT _ [] (AppT _ v)) Nothing -> return $ v

getFieldsTypeVar :: [Name] -> Q [Type]
getFieldsTypeVar names = (mapM getFieldTypeVar names)

generateTargetFields' :: SpliceArgs -> [(TyVarBndr, Name)] -> [VarBangType] -> ([VarBangType], [VarBangType])
generateTargetFields' args assocList fields = (targetNewFields, targetDeltaFields)
                      where
                        gen _ [] = []
                        gen reqFields ((v, b, t) : vbts) = if v `elem` reqFields
                                                                then ((sourceToTargetName args v,b,getFieldType assocList t) : gen reqFields vbts)
                                                                else gen reqFields vbts

                        genDelta _ [] = []
                        genDelta reqFields ((v,b,t) : vbts) = if v `elem` reqFields
                                                                              then genDelta reqFields vbts
                                                                              else (sourceToTargetName args v, b, getFieldType assocList t) : genDelta reqFields vbts
                        reqFields = requiredFields args
                        targetNewFields = gen reqFields fields
                        targetDeltaFields = genDelta reqFields fields
