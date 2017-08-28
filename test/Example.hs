{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens.TH
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

data Validated
data UnValidated

data T a b = T { _t :: Int, _g :: b, _h :: Integer } deriving (Show, Eq)

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
--------------------------------------------------------------

ts :: Tag
ts = TagPoly 3 4 "a" "b" 5 "c"

dt :: T Validated Int
dt = T 3 4 5

main :: IO ()
main = do
  putStrLn $ show $ tagNewToTag (tagToTagNew ts) (tagToTagNewDelta ts) == ts
  putStrLn $ show $ tINewToT (tToTINew dt) (tToTINewDelta dt) == dt