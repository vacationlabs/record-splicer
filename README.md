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
  }

createRecordSplice SpliceArgs
  {
    sourcePrefix = "_tag"
     -- NOTE: A type synonym is being used here. This has to be handled properly in the TH code
  , source = ''Tag
  , requiredFields = ['_tagName, '_tagColourCode]
  , targetName = "TagNew"
  , targetPrefix = "_tagn"
  , generateClassyLenses = True
  }
```

