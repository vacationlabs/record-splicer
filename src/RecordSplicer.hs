{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module RecordSplicer (SpliceArgs(..), createRecordSplice, HasSplice(..), IsMergeable(..)) where

import Control.Monad
import Control.Lens
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

class HasSplice a b where
  patch :: Lens' a b

class IsMergeable a b c | a b -> c where
  merge :: a -> b -> c

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
      let (ctx, name, tVars, kind, fields, classes) =
            case dataInfo of
                TyConI (DataD ctx name tVars kind [RecC _ fields] classes) ->
                  (ctx, name, tVars, kind, fields, classes)
          -- An associative list of the type variables in data types and the exact types
          -- in the type synonym
          assocListNamesTyVars =  getTypeVars tVars (drop 1 paramTypes)

      return $ constructDeclarations ctx name tyVars kind fields assocListNamesTyVars

    TyConI (DataD ctx name tyVars kind [RecC _ fields] classes) ->
      return $ constructDeclarations ctx name tyVars kind fields []
  where
    tName = mkName targetName
    dName = mkName $ targetName ++ "Delta"
    fsName = mkName $ (firstChar toLower . nameBase) source ++ "To" ++ targetName
    fdName = mkName $ (firstChar toLower . nameBase) source ++ "To" ++ nameBase dName
    fmName = mkName $ firstChar toLower targetName ++ "To" ++ nameBase source
    tVariable = mkName "t"
    dVariable = mkName "d"
    fVariable = mkName "f"
    ffmap = mkName "fmap"
    hasSpliceClass = mkName "HasSplice"
    isMergeableClass = mkName "IsMergeable"
    fpatch = mkName "patch"
    fmerge = mkName "merge"

    getRecConE :: [VarBangType] -> (Name -> Name) -> (Name -> Name) -> Name -> [(Name, Exp)]
    getRecConE [] _ _ _ = []
    getRecConE ((tFieldName, _, _) : vbts) f g n =
      (f tFieldName, AppE (VarE $ g tFieldName) (VarE n)) : getRecConE vbts f g n

    transformTyVars :: TyVarBndr -> TyVarBndr
    transformTyVars (KindedTV name kind) = PlainTV ((mkName . nameBase) name)

    kindedTyVarsToTypes :: [TyVarBndr] -> [Type]
    kindedTyVarsToTypes ((KindedTV n _):tvbs) = (VarT $ mkName . nameBase $ n) : kindedTyVarsToTypes tvbs
    kindedTyVarsToTypes ((PlainTV n):tvbs) = (VarT $ mkName . nameBase $ n) : kindedTyVarsToTypes tvbs
    kindedTyVarsToTypes _ = []

    makePlainTyVars :: Name -> TyVarBndr
    makePlainTyVars = PlainTV

    getTyVars :: [VarBangType] -> [Name]
    getTyVars [] = []
    getTyVars ((_,_,VarT n) : vbts) = n : getTyVars vbts
    getTyVars (_:vbts) = getTyVars vbts

    getPhantomTyVars :: [TyVarBndr] -> [Name] -> [Name]
    getPhantomTyVars ((KindedTV name _) : tvbs) names = ifte name names getPhantomTyVars tvbs
    getPhantomTyVars ((PlainTV name) : tvbs) names = ifte name names getPhantomTyVars tvbs
    getPhantomTyVars _ _ = []

    ifte n ns f tvbs = if n `elem` ns
                       then f tvbs ns
                       else n : f tvbs ns

    createTySigD :: Name -> [Type] -> Type
    createTySigD h types= foldl1 AppT $ ConT h : types

    constructDeclarations ctx name tyVars kind fields assocList =
      [
        DataD ctx tName targetParamVars kind [RecC tName targetFields] (map ConT deriveClasses)
      , DataD ctx dName deltaParamVars kind [RecC dName deltaFields] (map ConT deriveClasses)
      , SigD fsName (AppT (AppT ArrowT (createTySigD source $ kindedTyVarsToTypes tyVars)) (createTySigD tName $ kindedTyVarsToTypes targetParamVars))
      , FunD fsName [Clause [VarP tVariable]
                     (NormalB $ RecConE tName $
                      getRecConE targetFields id (targetToSourceName args) tVariable) []]
      , SigD fdName (AppT (AppT ArrowT (createTySigD source $ kindedTyVarsToTypes tyVars)) (createTySigD dName $ kindedTyVarsToTypes deltaParamVars))
      , FunD fdName [Clause [VarP dVariable]
                     (NormalB $ RecConE dName $
                      getRecConE deltaFields id (targetToSourceName args ) dVariable) []]
      , SigD fmName (AppT (AppT ArrowT (createTySigD tName $ kindedTyVarsToTypes targetParamVars))
                     (AppT (AppT ArrowT (createTySigD dName $ kindedTyVarsToTypes deltaParamVars)) (createTySigD source $ kindedTyVarsToTypes tyVars)))
      , FunD fmName [Clause [VarP tVariable, VarP dVariable]
                      (NormalB $ RecConE ((mkName . nameBase) name) $
                      getRecConE targetFields (targetToSourceName args) id tVariable ++
                      getRecConE deltaFields (targetToSourceName args) id dVariable) []]
      , InstanceD Nothing [] (AppT (AppT (ConT hasSpliceClass) (createTySigD source $ kindedTyVarsToTypes tyVars)) (createTySigD tName $ kindedTyVarsToTypes targetParamVars)) [
        FunD fpatch [Clause [VarP fVariable, VarP tVariable]
                     (NormalB $ AppE (AppE (VarE ffmap) (LamE [VarP dVariable] (RecUpdE (VarE tVariable) $ getRecConE targetFields (targetToSourceName args) id dVariable)))
                      (AppE (VarE fVariable) (AppE (LamE [VarP dVariable] (RecConE tName $ getRecConE targetFields id (targetToSourceName args) tVariable)) (VarE tVariable)))) []]]
      , InstanceD Nothing [] (AppT (AppT (ConT hasSpliceClass) (createTySigD source $ kindedTyVarsToTypes tyVars)) (createTySigD dName $ kindedTyVarsToTypes deltaParamVars)) [
        FunD fpatch [Clause [VarP fVariable, VarP tVariable]
                     (NormalB $ AppE (AppE (VarE ffmap) (LamE [VarP dVariable] (RecUpdE (VarE tVariable) $ getRecConE deltaFields (targetToSourceName args) id dVariable)))
                      (AppE (VarE fVariable) (AppE (LamE [VarP dVariable] (RecConE dName $ getRecConE deltaFields id (targetToSourceName args) tVariable)) (VarE tVariable)))) []]]
      , InstanceD Nothing [] (AppT (AppT (AppT (ConT isMergeableClass) (createTySigD tName $ kindedTyVarsToTypes targetParamVars))
                                    (createTySigD dName $ kindedTyVarsToTypes deltaParamVars)) (createTySigD source $ kindedTyVarsToTypes tyVars)) [
        FunD fmerge [Clause [VarP tVariable, VarP dVariable]
                                (NormalB $ RecConE ((mkName . nameBase) name) $
                                getRecConE targetFields (targetToSourceName args) id tVariable ++
                                getRecConE deltaFields (targetToSourceName args) id dVariable) []]]
      ]
      where
         phantomTyVars = getPhantomTyVars tyVars $ getTyVars fields
         (targetFields, deltaFields) = generateTargetFields args assocList fields
         targetParamVars = map makePlainTyVars (getTyVars targetFields) ++ map makePlainTyVars phantomTyVars
         deltaParamVars = map makePlainTyVars (getTyVars deltaFields) ++ map makePlainTyVars phantomTyVars

sourceToTargetName :: SpliceArgs -> Name -> Name
sourceToTargetName SpliceArgs{..} name =
  mkName $ targetPrefix ++ drop (length sourcePrefix) (nameBase name)

targetToSourceName :: SpliceArgs -> Name -> Name
targetToSourceName SpliceArgs{..} name =
  mkName $ sourcePrefix ++ drop (length targetPrefix) (nameBase name)

getTypes :: Type -> [Type]
-- getTypes (AppT (ConT cons) (ConT var)) = [var]
getTypes (AppT x t@(AppT y z)) = getTypes x ++ [t]
getTypes (AppT x y) = getTypes x ++ getTypes y
getTypes t@(ConT name) = [t]
getTypes t@(VarT name) = [t]

getFieldType :: [(TyVarBndr, Type)] -> Type -> Type
getFieldType ((KindedTV n1 kind, t1) : xs) t2@(VarT n2) | n1 == n2 = t1
                                                        | otherwise = getFieldType xs t2
getFieldType _ t@(ConT n2) = t
getFieldType _ t@(VarT n2) = t --VarT ((mkName . nameBase)n2)
getFieldType _ t@(AppT x y) = t

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
