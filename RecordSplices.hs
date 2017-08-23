module RecordSplices where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Foundation
import Data.List as DL
import Debug.Trace
import Data.List.Split as Split

-- spliceRecord :: Name -> [Name] -> Int
-- spliceRecord = undefined

data RecordSpliceArgs = RecordSpliceArgs
  {
    sourcePrefix :: String
  , sourceName :: Name
  , requiredFields :: [Name]
  , targetTypeString :: String
  , targetPrefix :: String
  }


createRecordSplice :: RecordSpliceArgs -> Q [Dec]
createRecordSplice args@RecordSpliceArgs{..} = reify sourceName >>= \case
  (TyConI (DataD [] recordConstr [] knd constrs classes)) -> do
    (targetTypeDecl, deltaTypeDecl) <- case constrs of
      [(RecC _ sourceFields)] -> constructTargetType args (sourceFields, knd, classes)
      _ -> fail $ "creating record splices for types with multiple constructors (sum types), i.e. data X = Y | Z, is not supported yet"

    pure $ [targetTypeDecl, deltaTypeDecl]

--    x = (''User, ['userFoo, 'userBar], "NewUser")
-- newUserToUser :: NewUser -> NewUserDeltaUser -> User
-- newUserToUser :: NewUser -> PK -> UTCTime -> UTCTime -> Status -> Sta
-- NewUserDeltaUser ={}
-- userToNewUser :: User -> (NewUser, NewUserdeltauser)

sourceToTargetName :: RecordSpliceArgs -> Name -> Name
sourceToTargetName RecordSpliceArgs{..} n = mkName $ targetPrefix ++ (DL.drop (DL.length sourcePrefix) (nameBase n))

sourceToDeltaName = sourceToTargetName

targetToSourceName :: RecordSpliceArgs -> Name -> Name
targetToSourceName RecordSpliceArgs{..} n = mkName $ sourcePrefix ++ (DL.drop (DL.length targetPrefix) (nameBase n))


constructTargetType :: RecordSpliceArgs -> ([VarBangType], Maybe Kind, Cxt) -> Q (Dec, Dec)
constructTargetType args@RecordSpliceArgs{..} (sourceFields, knd, classes) = do
  let (targetFields, deltaFields) = DL.foldl'
                  (\(tfields, dfields) (fname, bang_, ftype) ->
                      let cname = ":" ++ (DL.last $ Split.splitOn "." (nameBase fname)) ++ ":"
                      in if (DL.any (\x -> DL.isInfixOf cname x) stringifiedRequiredFields)
                         then ((sourceToTargetName args fname, bang_, ftype):tfields, dfields)
                         else (tfields, (sourceToDeltaName args fname, bang_, ftype):dfields)
                  ) ([], []) sourceFields
  pure $ (DataD [] targetName [] knd [RecC targetName (DL.reverse targetFields)] classes,
          DataD [] deltaName [] knd [RecC deltaName (DL.reverse deltaFields)] classes)
  where
    stringifiedRequiredFields = nameBase <$> requiredFields
    targetName = mkName targetTypeString
    deltaName = mkName $ targetTypeString ++ "Delta"


-- constructSourceToTargetFn :: RecordSpliceArgs -> VarBangType -> Dec -> Q Exp
-- constructSourceToTargetFn RecordSpliceArgs{..} (sourceFields, _, _) (DataD _ _ _ _ [RecC targetName targetFields] _) =
--   let fieldExps = DL.map
--                   (\(fname, _, _) -> fromJust $ DL.find (== fname) sourceFields)
--                   targetFields


-- class Record large small where
--   type RecordDelta large small
--   shrink :: large -> small
--   widen :: small -> (RecordDelta large small) -> large
