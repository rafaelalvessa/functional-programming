{-# LANGUAGE InstanceSigs, OverloadedStrings #-}

-- | This module provides functions to parse JSON files into Haskell data types.
module BooksParser where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Text.Internal

{-|
  The 'SearchResult' data type correponds to the structure of the JSON file
  produced by a Google Books search.
-}
data SearchResult = SearchResult {
  -- | The number of items found.
  totalItems :: Maybe Int,
  -- | A list contaning all the items found.
  items :: Maybe [Item]
} deriving (Show)

{-|
  The 'Item' data type corresponds to the nested JSON object @items@.
-}
data Item = Item {
  -- | An object contaning information about the volume.
  volumeInfo :: Maybe VolumeInfo
} deriving (Show)

{-|
The 'VolumeInfo' data type corresponds to the nested JSON object @volumeInfo@.
-}
data VolumeInfo = VolumeInfo {
  -- | The title of the book.
  title :: Maybe String,
  -- | The list containing the authors of the book.
  authors :: Maybe [String],
  -- | The book publisher.
  publisher :: Maybe String,
  -- | The date when the book was published.
  publishedDate :: Maybe String,
  -- | A list containing objects with the industry identifiers (ISBN codes).
  industryIdentifiers :: Maybe [IndustryIdentifier],
  -- | The number of pages the book has.
  pageCount :: Maybe Int,
  -- | The categories of the book.
  categories :: Maybe [String]
} deriving (Show)

{-|
  The 'IndustryIdentifier' data type corresponds to the elements of the list
  @industryIdentifiers@ that contains JSON objects with the ISBN-10 and ISBN-13
  codes.
-}
data IndustryIdentifier = IndustryIdentifier {
  -- | The type of ISBN code, i.e. ISBN-10 or ISBN-13.
  identifierType :: Maybe String,
  -- | The ISBN code that identifies the volume.
  identifier :: Maybe String
} deriving (Show)

{-|
  The 'AuthorInfo' data type correponds to the structure of the JSON file
  produced by a Freebase search.
-}
data AuthorInfo = AuthorInfo {
  -- | An object containing properties about the author.
  property :: Maybe Property
} deriving (Show)

{-|
  The 'Property' data type corresponds to the nested JSON object @property@.
-}
data Property = Property {
  -- | An object containing properties about the date of birth.
  dateOfBirth :: Maybe PropertyInfo,
  -- | An object containing properties about the gender.
  gender :: Maybe PropertyInfo,
  -- | An object contaning properties about the nationality.
  nationality :: Maybe PropertyInfo
} deriving (Show)

{-|
  The 'PropertyInfo' data type corresponds to the nested JSON objects
  @/@@people@@/@@person@@/@@date_of_birth@, @/@@people@@/@@person@@/@@gender@
  and @/@@people@@/@@person@@/@@nationality@.
-}
data PropertyInfo = PropertyInfo {
  -- | A list containing the values of the property.
  values :: Maybe [ValueInfo]
} deriving (Show)

{-|
  The 'ValueInfo' data type corresponds to the elements of the list
  @values@ that contains JSON objects with the corresponding value of date of
  birth, gender or nationality.
-}
data ValueInfo = ValueInfo {
  -- | The value of the property.
  text :: Maybe String
} deriving (Show)

instance FromJSON SearchResult where
  {-|
    This function is used to parse JSON objects that correspond to the
    'SearchResult' data type.
  -}
  parseJSON :: Value -> Parser SearchResult
  parseJSON (Object v) = SearchResult <$>
    v .:? "totalItems" <*>
    v .:? "items"
  parseJSON _ = mzero

instance FromJSON Item where
  {-|
    This function is used to parse JSON objects that correspond to the 'Item'
    data type.
  -}
  parseJSON :: Value -> Parser Item
  parseJSON (Object v) = Item <$>
    v .:? "volumeInfo"
  parseJSON _ = mzero

instance FromJSON VolumeInfo where
  {-|
    This function is used to parse JSON objects that correspond to the
    'VolumeItem' data type.
  -}
  parseJSON :: Value -> Parser VolumeInfo
  parseJSON (Object v) = VolumeInfo <$>
    v .:? "title" <*>
    v .:? "authors" <*>
    v .:? "publisher" <*>
    v .:? "publishedDate" <*>
    v .:? "industryIdentifiers" <*>
    v .:? "pageCount" <*>
    v .:? "categories"
  parseJSON _ = mzero

instance FromJSON IndustryIdentifier where
  {-|
    This function is used to parse JSON objects that correspond to the
    'IndustryIdentifier' data type.
  -}
  parseJSON :: Value -> Parser IndustryIdentifier
  parseJSON (Object v) = IndustryIdentifier <$>
    v .:? "type" <*>
    v .:? "identifier"
  parseJSON _ = mzero

instance FromJSON AuthorInfo where
  {-|
    This function is used to parse JSON objects that correspond to the
    'AuthorInfo' data type.
  -}
  parseJSON :: Value -> Parser AuthorInfo
  parseJSON (Object v) = AuthorInfo <$>
    v .:? "property"
  parseJSON _ = mzero

instance FromJSON Property where
  {-|
    This function is used to parse JSON objects that correspond to the
    'Property' data type.
  -}
  parseJSON :: Value -> Parser Property
  parseJSON (Object v) = Property <$>
    v .:? "/people/person/date_of_birth" <*>
    v .:? "/people/person/gender" <*>
    v .:? "/people/person/nationality"
  parseJSON _ = mzero

instance FromJSON PropertyInfo where
  {-|
    This function is used to parse JSON objects that correspond to the 'Value'
    data type.
  -}
  parseJSON :: Value -> Parser PropertyInfo
  parseJSON (Object v) = PropertyInfo <$>
    v .:? "values"
  parseJSON _ = mzero

instance FromJSON ValueInfo where
  {-|
    This function is used to parse JSON objects that correspond to the
    'ValueInfo' data type.
  -}
  parseJSON :: Value -> Parser ValueInfo
  parseJSON (Object v) = ValueInfo <$>
    v .:? "text"
  parseJSON _ = mzero
