# record-splicer
Utility TH functions to create sub-record-types from large record-types and function to convert values between these record-types

# How to use

```Haskell
data TagPoly id clientId name colourCode createdAt updatedAt = TagPoly {_tagId :: id, _tagClientId :: clientId, _tagName :: name, _tagColourCode :: colourCode, _tagCreatedAt :: createdAt, _tagUpdatedAt :: updatedAt} deriving (Eq, Show, Generic)
type Tag = TagPoly (TagId) (Integer) (Text) (Text) (UTCTime) (UTCTime)

createRecordSplice SpliceArgs
  {
    sourcePrefix = "_tag"
     -- NOTE: A type synonym is being used here. This has to be handled properly in the TH code
  , source = ''Tag
  , requiredFields = ['_tagClientId, '_tagName, '_tagColourCode]
  , targetName = "TagNew"
  , targetPrefix = "_tagn"
  , deriveClasses = [''Eq, ''Show, ''Generic]
  }
```

This will generate the following code (not the actual code, but the equivalent code) via TH:

```Haskell
data TagNew = TagNew
  {
    _tagnClientId :: Integer
  , _tagnName :: Text -- same datatype as Tag.name is used
  , _tagnColourCode :: Text -- same datatype as Tag.colourCode is used
  } deriving (Eq, Show, Generic)
  
-- NOTE: You will be unable to simply output this via TH. Running a TH splice 
-- from within a TH splice where the inner-splice depends on a type created by
-- the outer splice does not work. Some stupid TH restrictions.
makeLensesWith abbreviatedFields ''TagNew

data TagNewDelta = TagNewDelta
  {
     _tagnId :: TagId
   , _tagnCreatedAt :: UTCTime
   , _tagnUpdatedAt :: UTCTime
   } deriving (Eq, Show, Generic)

tagToTagNew :: Tag -> TagNew
tagToTagNew t = TagNew
  {
    _tagnClientId = (_tagClientId t)
  , _tagnName = (_tagName t)
  , _tagColourCode = (_tagColourCode t)
  }

tagToTagNewDelta :: Tag -> TagNewDelta
tagToTagNewDelta d = TagNewDelta
  {
    _tagnName = _tagName d
  , _tagnColourCode = _tagColourCode d
  , _tagnCreatedAt = _tagCreatedat d
  }
  
tagNewToTag :: TagNew -> TagNewDelta -> Tag
tagNewToTag t d = TagPoly
  {
    _tagId = (_tagnId d)
  , _tagClientId = (_tagnClientId t)
  , _tagName = (_tagnName t)
  , _tagColourCode = (_tagnColourCode t)
  , _tagCreatedAt = (_tagnCreatedAt d)
  , _tagUpdatedAt = (_tagUpdatedAt d)
  }

-- NOTE : These are equivalent instances, the actual instances generated do not use the above functions.

instance HasSplice Tag TagNew where
  patch f t
        = fmap
            (flip tagNewToTag (tagToTagNewDelta t))
            (f tagToTagNew t)

instance HasSplice Tag TagNewDelta where
  patch f t = fmap
              (tagNewToTag (tagToTagNew t))
              (f tagToTagNewDelta t)

instance IsMergeable TagNew TagNewDelta Tag where
  merge t d = TagPoly
              {_tagId = _tagnId t, _tagClientId = _tagnClientId t,
              _tagUpdatedAt = _tagnUpdatedAt t, _tagName = _tagnName d,
              _tagColourCode = _tagnColourCode d,
              _tagCreatedAt = _tagnCreatedAt d}
 
```

The `HasSplice` class has one method, `patch` which is a lens, and hence can be used as a getter or a setter.

```Haskell
class HasSplice a b where
  patch :: Lens' a b
```

`createRecordSplice` can splice regular Polymorphic Record data types and type synonyms of them,
it can also splice phantomesque data types.
