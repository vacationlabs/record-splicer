{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
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

type Tag a b = TagPoly a (Maybe Integer) (Maybe String) (Maybe String) (Maybe Int) (Maybe String)

createRecordSplice SpliceArgs
  {
     sourcePrefix = "_tag"
  ,  source = ''Tag
  ,  requiredFields = ['_tagId, '_tagClientId, '_tagUpdatedAt]
  ,  targetName = "TagNew"
  ,  targetPrefix = "_tagn"
  ,  deriveClasses = [''Eq, ''Show]
  ,  extrasFrom = Nothing
  }

data Validated
data UnValidated

data T a b = T { _t :: Maybe Int, _g :: b, _h :: Integer } deriving (Show, Eq)

createRecordSplice SpliceArgs
  {
    sourcePrefix = "_"
  , source = ''T
  , requiredFields = ['_t, '_h]
  , targetName = "TINew"
  , targetPrefix = "_n"
  , deriveClasses = [''Eq, ''Show]
  , extrasFrom = Nothing
  }

--------------------------------------------------------------
-- Property: merge (toTarget s) (s ^. targetDelta) == s    --
--------------------------------------------------------------

ts :: Tag String Validated
ts = TagPoly "String" (Just 4) (Just "a") (Just "b") (Just 5) (Just "c")

dt :: T Validated Int
dt = T (Just 3) 4 5

main :: IO ()
main = do
  putStrLn $ show $ merge (tagToTagNew ts :: TagNew String Validated) (ts ^. tagNewDelta :: TagNewDelta Validated) == ts
  putStrLn $ show $ merge (tToTINew dt) (dt ^. tINewDelta) == dt
