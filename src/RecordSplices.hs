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
          paramTypeNames = getTypes ty

      -- The data type info of the type synonym
      dataInfo <- reify $ head paramTypeNames

      -- The type variables in the data type
      let (ctx, paramVars, kind, fields, classes) = case dataInfo of
                                                        TyConI (DataD ctx name tVars kind [RecC _ fields] classes)-> (ctx, tVars, kind, fields, classes)
          -- An associative list of the type variables in data types and the exact types
          -- in the type synonym
          assocListNamesTyVars =  getTypeVars paramVars (drop 1 paramTypeNames)

          (targetFields, deltaFields) = generateTargetFields' args assocListNamesTyVars fields

      return
        [
          DataD ctx tName [] kind [RecC tName targetFields] classes
        , DataD ctx dName [] kind [RecC dName deltaFields] classes
        ]

sourceToTargetName :: SpliceArgs -> Name -> Name
sourceToTargetName SpliceArgs{..} name =
  mkName $ targetPrefix ++ DL.drop (DL.length sourcePrefix) (nameBase name)


getTypes :: Type -> [Name]
-- getTypes (AppT (ConT cons) (ConT var)) = [var]
getTypes (AppT x y) = getTypes x ++ getTypes y
getTypes (ConT name) = [name]

getFieldType :: [(TyVarBndr, Name)] -> Type -> Type
getFieldType ((KindedTV n1 kind, name) : xs) t@(VarT n2) | n1 == n2 = ConT name
                                                           | otherwise = getFieldType xs t
getFieldType _ t@(ConT n2) = t

getTypeVars :: [TyVarBndr] -> [Name] -> [(TyVarBndr, Name)]
getTypeVars = zip

generateTargetFields' :: SpliceArgs -> [(TyVarBndr, Name)] -> [VarBangType] -> ([VarBangType], [VarBangType])
generateTargetFields' args assocList fields = gen' reqFields fields
                      where
                        gen' :: [Name] -> [VarBangType] -> ([VarBangType],[VarBangType])
                        gen' _ [] = ([],[])
                        gen' reqFields ((v,b,t):vbts) = if v `elem` reqFields
                                                           then ((sourceToTargetName args v, b, getFieldType assocList t) : fst (gen' reqFields vbts), snd $ gen' reqFields vbts)
                                                           else (fst $ gen' reqFields vbts, (sourceToTargetName args v, b, getFieldType assocList t) : snd (gen' reqFields vbts))
                        reqFields = requiredFields args
