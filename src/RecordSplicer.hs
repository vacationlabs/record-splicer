{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RecordSplicer
  ( SpliceArgs(..), createRecordSplice, Patch(..), Merge(..)
  -- * Shared record-reflection core (reused by the splicer and the drift gates)
  , ReifiedRecord(..)
  , reifyRecord
  , recordFieldPartition
  , checkNameSet
  , getNameFromField
  ) where

import Control.Monad
import Control.Lens
import Data.Char (toUpper, toLower)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data SpliceArgs = SpliceArgs
     { sourcePrefix :: String
     , source :: Name
     , requiredFields :: [Name]
     , targetName :: String
     , targetPrefix :: String
     , deriveClasses :: [Name]
     , extrasFrom :: Maybe Name
     -- ^ Optional: a user-defined record type whose fields are appended to the target.
     -- RecordSplicer reifies this type to extract its fields. The user-defined type is
     -- used directly in the generated 'sourceToTargetWith' function signature.
     -- When 'Nothing', no extras are added (backward-compatible default).
     }

-- | Apply the required fields of a target back onto a source record.
-- Generated as: patch :: target -> source -> source
class Patch a b where
  patch :: a -> b -> b

-- | Reconstruct a source record from a target and its delta (complement).
-- Generated as: merge :: target -> delta -> source
class Merge a b c | a b -> c where
  merge :: a -> b -> c

createRecordSplice :: SpliceArgs -> Q [Dec]
createRecordSplice args@SpliceArgs{..} = do
  -- Reify the extras type if provided
  (mExtrasInfo, resolvedExtraFields) <- case extrasFrom of
    Nothing -> pure (Nothing, [])
    Just extrasName -> do
      extrasInfo <- reify extrasName
      case extrasInfo of
        TyConI (DataD _ _ _ _ [RecC conName fields] _) ->
          -- Strip module qualification from field names so they work in generated code
          let unqualFields = [ (mkName (nameBase fn), b, t) | (fn, b, t) <- fields ]
          in pure (Just (conName, unqualFields), unqualFields)
        _ -> fail $ "extrasFrom: " ++ show extrasName ++ " must be a single-constructor record type"

  -- Generate unique local variable names for extras (avoids DuplicateRecordFields ambiguity)
  extraLocalNames <- forM resolvedExtraFields $ \(fn, _, _) -> newName ("ex_" ++ nameBase fn)

  -- Resolve the source record via the shared reflection core (synonym-aware).
  -- 'rrHeadTyVars' are the type variables exposed on the generated target/delta:
  -- the synonym's own (typically none, since a synonym fully applies the data
  -- constructor) or the data declaration's for a direct record. 'rrSynArgs' are
  -- the concrete types the synonym applied; empty for a direct data type.
  ReifiedRecord{..} <- reifyRecord source
  let assocList = getTypeVars rrDataTyVars rrSynArgs
      tVars'    = if Prelude.null rrSynArgs then getTyNamesFromFields rrFields else rrSynArgs
  return $ constructDeclarations mExtrasInfo resolvedExtraFields extraLocalNames rrCtx rrName rrHeadTyVars rrKind rrFields assocList tVars'
  where
    tName = mkName targetName
    dName = mkName $ targetName ++ "Delta"
    fsName = mkName $ (firstChar toLower . nameBase) source ++ "To" ++ targetName
    fsWithName = mkName $ (firstChar toLower . nameBase) source ++ "To" ++ targetName ++ "With"
    fdDeltaName = mkName $ firstChar toLower targetName ++ "Delta"
    tVariable = mkName "t"
    dVariable = mkName "d"
    sVariable = mkName "s"
    fVariable = mkName "f"
    ffmap = mkName "fmap"
    patchClass = ''Patch
    mergeClass = ''Merge
    fpatch = mkName "patch"
    fmerge = mkName "merge"
    lensType = ''Lens'

    getRecConE :: [VarBangType] -> (Name -> Name) -> (Name -> Name) -> Name -> [(Name, Exp)]
    getRecConE [] _ _ _ = []
    getRecConE ((tFieldName, _, _) : vbts) f g n =
      (f tFieldName, AppE (VarE $ g tFieldName) (VarE n)) : getRecConE vbts f g n

    kindedTyVarsToTypes :: [TyVarBndr ()] -> [Type]
    kindedTyVarsToTypes ((KindedTV n () _):tvbs) = (VarT $ mkName . nameBase $ n) : kindedTyVarsToTypes tvbs
    kindedTyVarsToTypes ((PlainTV n ()):tvbs) = (VarT $ mkName . nameBase $ n) : kindedTyVarsToTypes tvbs
    kindedTyVarsToTypes _ = []

    makePlainTyVars :: Name -> TyVarBndr ()
    makePlainTyVars n = PlainTV n ()

    getTyVars :: [VarBangType] -> [Name]
    getTyVars [] = []
    getTyVars ((_,_,VarT n) : vbts) = n : getTyVars vbts
    getTyVars (_:vbts) = getTyVars vbts

    getPhantomTyVars :: [TyVarBndr ()] -> [Name] -> [Name]
    getPhantomTyVars ((KindedTV name () _) : tvbs) names = ifte name names getPhantomTyVars tvbs
    getPhantomTyVars ((PlainTV name ()) : tvbs) names = ifte name names getPhantomTyVars tvbs
    getPhantomTyVars _ _ = []

    ifte n ns f tvbs = if n `elem` ns
                       then f tvbs ns
                       else n : f tvbs ns

    createTySigD :: Name -> [Type] -> Type
    createTySigD h types = foldl1 AppT $ ConT h : types

    constructDeclarations mExtrasInfo resolvedExtras extraLocalNames ctx name tyVars kind fields assocList tVars' =
      -- Target and delta data declarations (always generated)
      [ DataD ctx tName targetParamVars kind [RecC tName (targetFields ++ resolvedExtras)] [DerivClause Nothing (map ConT deriveClasses)]
      , DataD ctx dName deltaParamVars kind [RecC dName deltaFields] [DerivClause Nothing (map ConT deriveClasses)]
      -- {lowerTarget}Delta :: Lens' source delta
      -- Plain lens focusing on the complement (delta) fields within source.
      , SigD fdDeltaName (AppT (AppT (ConT lensType) (createTySigD source $ kindedTyVarsToTypes tyVars)) (createTySigD dName $ kindedTyVarsToTypes deltaParamVars))
      , FunD fdDeltaName [Clause [VarP fVariable, VarP tVariable]
          (NormalB $ AppE (AppE (VarE ffmap) (LamE [VarP dVariable] (RecUpdE (VarE tVariable) $ getRecConE deltaFields (targetToSourceName args) id dVariable)))
           (AppE (VarE fVariable) (RecConE dName $ getRecConE deltaFields id (targetToSourceName args) tVariable))) []]
      -- Merge target delta source
      , InstanceD Nothing [] (AppT (AppT (AppT (ConT mergeClass) (createTySigD tName $ kindedTyVarsToTypes targetParamVars))
                                    (createTySigD dName $ kindedTyVarsToTypes deltaParamVars)) (createTySigD source $ kindedTyVarsToTypes tyVars)) [
        FunD fmerge [Clause [VarP tVariable, VarP dVariable]
          (NormalB $ RecConE ((mkName . nameBase) name) $
          getRecConE targetFields (targetToSourceName args) id tVariable ++
          getRecConE deltaFields (targetToSourceName args) id dVariable) []]]
      -- Patch target source (always: apply required target fields back onto source)
      , InstanceD Nothing [] (AppT (AppT (ConT patchClass) (createTySigD tName $ kindedTyVarsToTypes targetParamVars)) (createTySigD source $ kindedTyVarsToTypes tyVars)) [
        FunD fpatch [Clause [VarP tVariable, VarP sVariable]
          (NormalB $ RecUpdE (VarE sVariable) $ getRecConE targetFields (targetToSourceName args) id tVariable) []]]
      ]
      -- sourceToTargetWith: source -> extras -> target (when extras exist)
      -- sourceToTarget: source -> target (when no extras)
      ++ case (extrasFrom, mExtrasInfo) of
        (Just extrasName, Just (extrasConName, _)) ->
          let extraRecPat = RecP extrasConName [ (fn, VarP ln) | ((fn, _, _), ln) <- zip resolvedExtras extraLocalNames ]
              extraRecConE = [ (fn, VarE ln) | ((fn, _, _), ln) <- zip resolvedExtras extraLocalNames ]
          in
          [ SigD fsWithName (AppT (AppT ArrowT (createTySigD source $ kindedTyVarsToTypes tyVars))
                            (AppT (AppT ArrowT (ConT extrasName))
                                  (createTySigD tName $ kindedTyVarsToTypes targetParamVars)))
          , FunD fsWithName [Clause [VarP tVariable, extraRecPat]
                             (NormalB $ RecConE tName $
                              getRecConE targetFields id (targetToSourceName args) tVariable ++
                              extraRecConE) []]
          ]
        _ ->
          -- No extras: can construct target from source alone
          [ SigD fsName (AppT (AppT ArrowT (createTySigD source $ kindedTyVarsToTypes tyVars)) (createTySigD tName $ kindedTyVarsToTypes targetParamVars))
          , FunD fsName [Clause [VarP tVariable]
                         (NormalB $ RecConE tName $
                          getRecConE targetFields id (targetToSourceName args) tVariable) []]
          ]
      where
         phantomTyVars = getPhantomTyVars tyVars $ getNames tVars'
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
getTypes (AppT x t@(AppT y z)) = getTypes x ++ [t]
getTypes (AppT x y) = getTypes x ++ getTypes y
getTypes t@(ConT name) = [t]
getTypes t@(VarT name) = [t]
getTypes t@(TupleT x) = [t]
getTypes x = error $ "Do not know how to handle type " ++ (show x)

getNameVars :: Type -> [Name]
getNameVars (VarT n) = [n]
getNameVars (AppT t1 t2) = getNameVars t1 ++ getNameVars t2
getNameVars _ = []

getNames :: [Type] -> [Name]
getNames ts = mconcat $ map getNameVars ts

getTyNamesFromField :: VarBangType -> Type
getTyNamesFromField (_, _, t) = t

getTyNamesFromFields :: [VarBangType] -> [Type]
getTyNamesFromFields vbts = map getTyNamesFromField vbts

getFieldType :: [(TyVarBndr (), Type)] -> Type -> Type
getFieldType ((KindedTV n1 () kind, t1) : xs) t2@(VarT n2) | n1 == n2 = t1
                                                        | otherwise = getFieldType xs t2
getFieldType _ t@(ConT n2) = t
getFieldType _ t@(VarT n2) = t
getFieldType _ t@(AppT x y) = t

getTypeVars :: [TyVarBndr ()] -> [Type] -> [(TyVarBndr (), Type)]
getTypeVars = zip

generateTargetFields :: SpliceArgs -> [(TyVarBndr (), Type)] -> [VarBangType] -> ([VarBangType], [VarBangType])
generateTargetFields args assocList = gen' reqFields
  where
    gen' :: [Name] -> [VarBangType] -> ([VarBangType],[VarBangType])
    gen' _ [] = ([],[])
    gen' reqFields ((v,b,t):vbts) =
      if ((mkName . nameBase) v) `elem` reqFields
      then ((sourceToTargetName args v, b, getFieldType assocList t) : fst (gen' reqFields vbts),
             snd $ gen' reqFields vbts)
      else (fst $ gen' reqFields vbts,
            (sourceToTargetName args v, b, getFieldType assocList t) : snd (gen' reqFields vbts))
    reqFields = map getNameFromField $ requiredFields args

getNameFromField :: Name -> Name
getNameFromField field = if head fieldName == '$'
                            then mkName $ takeWhile (\c -> c /= ':') (drop 5 fieldName)
                            else mkName $ fieldName
  where
    fieldName = nameBase field

firstChar :: (Char -> Char) -> String -> String
firstChar f name = (f . head) name : tail name

-- ============================================================
-- Shared record-reflection core + compile-time drift gates
--
-- 'reifyRecord' is the single resolver from a (possibly type-synonym) name down
-- to its underlying single-record data declaration. Both the splicer
-- ('createRecordSplice') and the drift gate ('recordFieldPartition') consume it,
-- each destructuring the fields it needs -- so the synonym-resolution logic lives
-- in exactly one place.
-- ============================================================

-- | The resolved shape of a single-constructor record type, with a type-synonym
-- (e.g. @type Itinerary = ItineraryPoly ...@) followed down to its underlying
-- data declaration.
data ReifiedRecord = ReifiedRecord
  { rrCtx        :: Cxt                -- ^ data declaration context
  , rrName       :: Name              -- ^ underlying data-type constructor name (e.g. @ItineraryPoly@)
  , rrDataTyVars :: [TyVarBndr ()]    -- ^ the data declaration's own type variables
  , rrHeadTyVars :: [TyVarBndr ()]    -- ^ type vars to expose downstream: the synonym's (synonym case) or the data's (direct case)
  , rrKind       :: Maybe Kind
  , rrFields     :: [VarBangType]     -- ^ record fields, as declared on the data type
  , rrDeriv      :: [DerivClause]
  , rrSynArgs    :: [Type]            -- ^ concrete types the synonym applied; @[]@ for a direct data type
  }

-- | Resolve @nm@ to its 'ReifiedRecord', following a type synonym to the
-- underlying record. Fails for non-record / multi-constructor types.
reifyRecord :: Name -> Q ReifiedRecord
reifyRecord nm = do
  info <- reify nm
  case info of
    TyConI (TySynD _ synTyVars ty) ->
      case getTypes ty of
        (ConT d : appliedArgs) -> do
          (ctx, name, dataTyVars, kind, fields, classes) <- reifyDataDecl d
          pure ReifiedRecord
            { rrCtx = ctx, rrName = name, rrDataTyVars = dataTyVars
            , rrHeadTyVars = synTyVars, rrKind = kind, rrFields = fields
            , rrDeriv = classes, rrSynArgs = appliedArgs }
        _ -> fail $ "reifyRecord: cannot resolve type-synonym head of " ++ nameBase nm
    TyConI dec -> do
      (ctx, name, dataTyVars, kind, fields, classes) <- decRecord nm dec
      pure ReifiedRecord
        { rrCtx = ctx, rrName = name, rrDataTyVars = dataTyVars
        , rrHeadTyVars = dataTyVars, rrKind = kind, rrFields = fields
        , rrDeriv = classes, rrSynArgs = [] }
    _ -> fail $ "reifyRecord: " ++ nameBase nm ++ " is not a type constructor"

reifyDataDecl :: Name -> Q (Cxt, Name, [TyVarBndr ()], Maybe Kind, [VarBangType], [DerivClause])
reifyDataDecl d = do
  info <- reify d
  case info of
    TyConI dec -> decRecord d dec
    _          -> fail $ "reifyRecord: " ++ nameBase d ++ " is not a data type"

decRecord :: Name -> Dec -> Q (Cxt, Name, [TyVarBndr ()], Maybe Kind, [VarBangType], [DerivClause])
decRecord _  (DataD ctx name tyVars kind [RecC _ fields] classes)    = pure (ctx, name, tyVars, kind, fields, classes)
decRecord _  (NewtypeD ctx name tyVars kind (RecC _ fields) classes) = pure (ctx, name, tyVars, kind, fields, classes)
decRecord nm _                                                       = fail $ "reifyRecord: " ++ nameBase nm ++ " is not a single-constructor record type"

-- | Diff a developer-maintained @expected@ name set against the @actual@ names
-- found on a type by 'reify', failing the build (with @note@) on any drift.
-- @kind@ ("field" / "constructor") and @tyName@ only shape the error message.
-- Shared by 'recordFieldPartition' and any constructor-set variant.
checkNameSet :: String -> String -> [String] -> [String] -> String -> Q ()
checkNameSet kind tyName expected actual note = do
  let added   = filter (`notElem` expected) actual   -- on the type, not yet classified
      removed = filter (`notElem` actual) expected     -- classified, but gone from the type
  unless (Prelude.null added && Prelude.null removed) $
    fail $ unlines
      [ "Compile-time " ++ kind ++ " guard: the " ++ kind ++ " set of " ++ tyName ++ " has drifted."
      , "  Unclassified (present on the type, missing from your lists): " ++ show added
      , "  Stale (in your lists, no longer present on the type):        " ++ show removed
      , "Action required: " ++ note
      ]

-- | Compile-time guard: every record field of @typeName@ (autogen @*Poly@
-- synonyms supported) MUST be classified as either kept (@whitelist@) or
-- dropped (@blacklist@). A newly-added field that appears in neither list fails
-- the build; a field listed in BOTH fails the build. Emits no declarations.
--
-- Fields are passed as 'Name's via TH name-quotes (@'_itineraryName@), which
-- gives a second, independent check for free: each listed selector must be in
-- scope, so a typo'd or deleted field is a "not in scope" error at the call-site
-- before this guard even runs. The 'reify' pass adds the completeness check the
-- quotes cannot: that every actual field is classified exactly once.
--
-- Example:
--   $(recordFieldPartition ''Itinerary
--       [ '_itineraryName, '_itineraryOverview {- ...sent downstream... -} ]
--       [ '_itineraryId, '_itineraryTotalPrice {- ...deliberately withheld... -} ]
--       "Classify the new itineraries column in Plugin.ItineraryBuilder.LLMFormatters.")
recordFieldPartition :: Name -> [Name] -> [Name] -> String -> Q [Dec]
recordFieldPartition typeName whitelist blacklist note = do
  fields <- rrFields <$> reifyRecord typeName
  -- 'getNameFromField' normalises the DuplicateRecordFields mangling: a quoted
  -- selector ''_itineraryName'' reifies with nameBase "$sel:_itineraryName:ItineraryPoly",
  -- which must be reduced to the bare "_itineraryName" to match the field names.
  let actual    = map (\(fn, _, _) -> nameBase fn) fields
      keep      = map (nameBase . getNameFromField) whitelist
      drop_     = map (nameBase . getNameFromField) blacklist
      both      = filter (`elem` drop_) keep
  unless (Prelude.null both) $
    fail $ unlines
      [ "recordFieldPartition: field(s) classified as BOTH kept and dropped on " ++ nameBase typeName ++ ":"
      , "  " ++ show both
      , "Action required: " ++ note
      ]
  checkNameSet "field" (nameBase typeName) (keep ++ drop_) actual note
  pure []
