{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import RecordSplicer

data TagPoly tagId clientId name colourCode createdAt updatedAt =
  TagPoly {
             _tagId :: tagId
           , _tagClientId :: clientId
           , _tagName :: name
           , _tagColourCode :: colourCode
           , _tagCreatedAt :: createdAt
           , _tagUpdatedAt :: updatedAt
          } deriving (Eq, Show)

type Tag = TagPoly Int Integer String String Int String

createRecordSplice SpliceArgs
  {
     sourcePrefix = "_tag"
  ,  source = ''Tag
  ,  requiredFields = ['_tagId, '_tagClientId, '_tagUpdatedAt]
  ,  targetName = "TagNew"
  ,  targetPrefix = "_tagn"
  ,  generateClassyLenses = True
  ,  deriveClasses = [''Eq, ''Show]
  }

data Validate = Validated | UnValidated

data T a = T { _t :: a, _g :: String, _h :: Integer } deriving (Show, Eq)
type TI = T Validate

createRecordSplice SpliceArgs
  {
    sourcePrefix = "_"
  , source = ''T
  , requiredFields = ['_t, '_h]
  , targetName = "TINew"
  , targetPrefix = "_n"
  , generateClassyLenses = True
  , deriveClasses = [''Eq, ''Show]
  }
--------------------------------------------------------------
-- The following property should hold                       --
-- tagNewToTag (tagToTagNew tp) (tagToTagNewDelta tp) == tp --
-------------------------------------------------------------- main :: IO ()

tp :: Tag
tp = TagPoly 3 4 "a" "b" 5 "c"

main :: IO ()
main = putStrLn $ show $ tagNewToTag (tagToTagNew tp) (tagToTagNewDelta tp) == tp
