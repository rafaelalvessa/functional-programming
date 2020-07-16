-- | This module formats 'URL's according to the APIs and performs HTTP
-- requests.
module BooksNetwork (downloadURL, formatAuthorsURL, formatBooksURL, URL) where

import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
import qualified Data.Text as T
import Data.Word
import Language.Haskell.TH.Ppr
import Network.HTTP.Conduit

-- | A @String@ representing a URL.
type URL = String

{-|
  This function takes a @String@ with the search terms and returns a 'URL' in
  the format required by the Google Books API.
-}
formatBooksURL :: String -> Int -> URL
formatBooksURL terms index = "https://www.googleapis.com/books/v1/volumes?q=" ++
    "inauthor:" ++ (formatTerms (removeSpaces terms) '+') ++
    "&printType=books&fields=totalItems,items(volumeInfo(title,authors," ++
    "publisher,publishedDate,industryIdentifiers,pageCount,categories))" ++
    "&maxResults=40&startIndex=" ++ (show index)

{-|
  This function takes a @String@ with the search terms and returns a 'URL' in
  the format required by the Freebase API.
-}
formatAuthorsURL :: String -> URL
formatAuthorsURL terms = "https://www.googleapis.com/freebase/v1/topic/en/" ++
    (formatTerms (removeSpaces terms) '_') ++ "?filter=/people/person/" ++
    "date_of_birth&filter=/people/person/gender&filter=/people/person/" ++
    "nationality"


{-|
  This function takes a @String@ and returns a list of @String@s, each
  containing a single word without blank spaces surrounding it.
-}
removeSpaces :: String -> [String]
removeSpaces terms = [T.unpack x | x <- T.split (== ' ') (T.strip . T.toLower .
                      T.pack $ terms), x /= T.empty]

{-|
  This function takes a list of @String@s and a @Char@, and returns all the
  elements separated by the specified character.
-}
formatTerms :: [String] -> Char -> String
formatTerms [] _ = ""
formatTerms [x] c = x
formatTerms (x:xs) c = x ++ c : (formatTerms xs c)

{-|
  This function takes a 'URL', makes an HTTP request and returns its downloaded
  content as a @ByteString@.
-}
downloadURL :: URL -> IO BS.ByteString
downloadURL url = do
  request <- try (simpleHttp url) :: IO (Either HttpException BS.ByteString)
  let result = lefts [request]
  if length result == 0 then do
    let content = head . rights $ [request]
    return content
  else
    return (BS.pack "")
