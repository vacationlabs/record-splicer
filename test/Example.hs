{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Lens.TH
import Control.Lens.Type
import Control.Lens.Internal
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

type Tag = TagPoly (Either String Int) (Maybe Integer) (Maybe String) (Maybe String) (Maybe Int) (Maybe String)


createRecordSplice SpliceArgs
  {
     sourcePrefix = "_tag"
  ,  source = ''Tag
  ,  requiredFields = ['_tagId, '_tagClientId, '_tagUpdatedAt]
  ,  targetName = "TagNew"
  ,  targetPrefix = "_tagn"
  ,  deriveClasses = [''Eq, ''Show]
  }

-- makeLenses ''TagNew

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
  }
--------------------------------------------------------------
-- The following property should hold                       --
-- tagNewToTag (tagToTagNew tp) (tagToTagNewDelta tp) == tp --
--------------------------------------------------------------

ts :: Tag
ts = TagPoly (Right 3) (Just 4) (Just "a") (Just "b") (Just 5) (Just "c")

dt :: T Validated Int
dt = T (Just 3) 4 5

main :: IO ()
main = do
  putStrLn $ show $ merge (ts ^. patch :: TagNew) (ts ^. patch :: TagNewDelta) == ts
  putStrLn $ show $ tINewToT (tToTINew dt) (tToTINewDelta dt) == dt
