{-# LANGUAGE RecordWildCards #-}
module RecordSplicer (SpliceArgs(..), createRecordSplice) where

import Control.Monad
import Data.Char (toUpper, toLower)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
createRecordSplice args@SpliceArgs{..} = do
  info <- reify source

  case info of
    TyConI (TySynD name tyVars ty) -> do
      let paramTypes = getTypes ty
          -- List of types used in the type synonym, with the data type as the first parameter

      -- The data type info of the type synonym
      dataInfo <- reify $ case head paramTypes of
                            ConT d -> d

      -- The type variables in the data type
      let (ctx, name, paramVars, kind, fields, classes) =
            case dataInfo of
                TyConI (DataD ctx name tVars kind [RecC _ fields] classes) ->
                  (ctx, name, tVars, kind, fields, classes)
          -- An associative list of the type variables in data types and the exact types
          -- in the type synonym
          assocListNamesTyVars =  getTypeVars paramVars (drop 1 paramTypes)

          (targetFields, deltaFields) = generateTargetFields args assocListNamesTyVars fields

      return
        [
          DataD ctx tName [] kind [RecC tName targetFields] [DerivClause Nothing $ map ConT deriveClasses]
        , DataD ctx dName [] kind [RecC dName deltaFields] [DerivClause Nothing $ map ConT deriveClasses]
        , SigD fsName (AppT (AppT ArrowT (ConT source)) (ConT tName))
        , FunD fsName [Clause [VarP fptName]
                       (NormalB $ RecConE tName $
                        getRecConE targetFields id (targetToSourceName args) fptName) []]
        , SigD fdName (AppT (AppT ArrowT (ConT source)) (ConT dName))
        , FunD fdName [Clause [VarP fpdName]
                       (NormalB $ RecConE dName $
                        getRecConE deltaFields id (targetToSourceName args ) fpdName) []]
        , SigD fmName (AppT (AppT ArrowT (ConT tName)) (AppT (AppT ArrowT (ConT dName)) (ConT source)))
        , FunD fmName [Clause [VarP fptName, VarP fpdName]
                        (NormalB $ RecConE ((mkName . nameBase) name) $
                        getRecConE targetFields (targetToSourceName args) id fptName ++
                        getRecConE deltaFields (targetToSourceName args) id fpdName) []]
        ]
    TyConI (DataD ctx name tyVars kind [RecC _ fields] classes) -> do
      let (targetFields, deltaFields) = generateTargetFields args [] fields
          paramVars = map transformTyVars tyVars

      return
        [
          DataD ctx tName paramVars kind [RecC tName targetFields] classes
        , DataD ctx dName paramVars kind [RecC dName deltaFields] classes
        , SigD fsName (AppT (AppT ArrowT (foldl1 AppT (ConT source :  (map (\(PlainTV x) -> (VarT x)) paramVars)))) (foldl1 AppT (ConT tName : (map (\(PlainTV x) -> (VarT x)) paramVars))))
        , FunD fsName [Clause [VarP fptName]
                           (NormalB $ RecConE tName $
                            getRecConE targetFields id (targetToSourceName args) fptName) []]
        ]
  where
    tName = mkName targetName
    dName = mkName $ targetName ++ "Delta"
    fsName = mkName $ (firstChar toLower . nameBase) source ++ "To" ++ targetName
    fdName = mkName $ (firstChar toLower . nameBase) source ++ "To" ++ nameBase dName
    fmName = mkName $ firstChar toLower targetName ++ "To" ++ nameBase source
    fptName = mkName "t"
    fpdName = mkName "d"

    getRecConE :: [VarBangType] -> (Name -> Name) -> (Name -> Name) -> Name -> [(Name, Exp)]
    getRecConE [] _ _ _ = []
    getRecConE ((tFieldName, _, _) : vbts) f g n =
      (f tFieldName, AppE (VarE $ g tFieldName) (VarE n)) : getRecConE vbts f g n

    transformTyVars :: TyVarBndr -> TyVarBndr
    transformTyVars (KindedTV name kind) = (PlainTV ((mkName . nameBase) name))

sourceToTargetName :: SpliceArgs -> Name -> Name
sourceToTargetName SpliceArgs{..} name =
  mkName $ targetPrefix ++ drop (length sourcePrefix) (nameBase name)

targetToSourceName :: SpliceArgs -> Name -> Name
targetToSourceName SpliceArgs{..} name =
  mkName $ sourcePrefix ++ drop (length targetPrefix) (nameBase name)

getTypes :: Type -> [Type]
-- getTypes (AppT (ConT cons) (ConT var)) = [var]
getTypes (AppT x y) = getTypes x ++ getTypes y
getTypes t@(ConT name) = [t]

getFieldType :: [(TyVarBndr, Type)] -> Type -> Type
getFieldType ((KindedTV n1 kind, t1) : xs) t2@(VarT n2) | n1 == n2 = t1
                                                        | otherwise = getFieldType xs t2
getFieldType _ t@(ConT n2) = t
getFieldType _ t@ (VarT n2) = VarT ((mkName . nameBase)n2)

getTypeVars :: [TyVarBndr] -> [Type] -> [(TyVarBndr, Type)]
getTypeVars = zip

generateTargetFields :: SpliceArgs -> [(TyVarBndr, Type)] -> [VarBangType] -> ([VarBangType], [VarBangType])
generateTargetFields args assocList = gen' reqFields
  where
    -- This function could probably be much succint
    gen' :: [Name] -> [VarBangType] -> ([VarBangType],[VarBangType])
    gen' _ [] = ([],[])
    gen' reqFields ((v,b,t):vbts) =
      if v `elem` reqFields
      then ((sourceToTargetName args v, b, getFieldType assocList t) : fst (gen' reqFields vbts),
             snd $ gen' reqFields vbts)
      else (fst $ gen' reqFields vbts,
            (sourceToTargetName args v, b, getFieldType assocList t) : snd (gen' reqFields vbts))
    reqFields = requiredFields args

firstChar :: (Char -> Char) -> String -> String
firstChar f name =  (f . head) name : tail name
