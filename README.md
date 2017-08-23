# record-splicer
Utility TH functions to create sub-record-types from large record-types and function to convert values between these record-types

# How to use

```
data TagPoly id clientId name colourCode createdAt updatedAt = TagPoly {_tagId :: id, _tagClientId :: clientId, _tagName :: name, _tagColourCode :: colourCode, _tagCreatedAt :: createdAt, _tagUpdatedAt :: updatedAt} deriving (Eq, Show, Generic)
type Tag = TagPoly (TagId) (Integer) (Text) (Text) (UTCTime) (UTCTime)

data SpliceArgs = SpliceArgs
  {
    sourcePrefix :: String
  , source :: Name
  , requiredFields :: [Name]
  , targetName :: String
  , targetPrefix :: String
  , generateClassyLenses :: Bool
  , deriveClasses :: [Name]
  }

createRecordSplice SpliceArgs
  {
    sourcePrefix = "_tag"
     -- NOTE: A type synonym is being used here. This has to be handled properly in the TH code
  , source = ''Tag
  , requiredFields = ['_tagClientId, '_tagName, '_tagColourCode]
  , targetName = "TagNew"
  , targetPrefix = "_tagn"
  , generateClassyLenses = True
  
  -- NOTE: If there is a way to figure out what classes `source` derives, then we don't really need this.
  , deriveClasses = [''Eq, ''Show, ''Generic]
  }
```

This will generate the following code (not the actualy code, but the equivalent code) via TH:

```
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

data TagNewDelta = TagNewDela
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
  
tagNewToTag :: TagNew -> TagNewDelta -> Tag
tagNewToTag t d = Tag
  {
    _tagId = (_tagnId d)
  , _tagClientId = (_tagnClientId t)
  , _tagName = (_tagnName t)
  , _tagColourCode = (_tagnName t)
  , _tagCreatedAt = (_tagnCreatedAt d)
  , _tagUpdatedAt = (_tagUpdatedAt d)
  }
```
