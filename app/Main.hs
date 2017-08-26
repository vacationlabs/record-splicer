{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Lib
import RecordSplices

main :: IO ()
main = someFunc

data TagPoly clientId name colourCode createdAt updatedAt =
  TagPoly {
             _tagId :: Int
           , _tagClientId :: clientId
           , _tagName :: name
           , _tagColourCode :: colourCode
           , _tagCreatedAt :: createdAt
           , _tagUpdatedAt :: updatedAt
          } deriving (Eq, Show)

type Tag = TagPoly Integer String String Int String

createRecordSplice SpliceArgs
  {
     sourcePrefix = "_tag"
  ,  source = ''Tag
  ,  requiredFields = ['_tagId, '_tagClientId, '_tagUpdatedAt]
  ,  targetName = "TagNew"
  ,  targetPrefix = "_tagn"
  ,  generateClassyLenses = True
  ,  deriveClasses = []
  }
