{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import Control.Applicative
import Control.Monad
import Data.Char
import System.IO
import System.IO.Error
import System.Directory

{-|
  = __Part 1: Parse JSON input into Haskell data type (using Aeson).__
-}

{-|
  The Dataset data type corresponds to the structure of the JSON sample file
  used for this project, 'youtube.json', which represents a YouTube video
  search.
-}
data Dataset = Dataset {
  -- | This field corresponds to the version of the API.
  apiVersion :: String,
  -- | This field corresponds to the resultsData JSON nexted object.
  resultsData :: ResultData
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the
  Dataset data type.
-}
instance FromJSON Dataset where
  parseJSON (Object v) = Dataset <$>
    v .: "apiVersion" <*>
    parseJSON (fromJust $ HMS.lookup "resultsData" v)
  parseJSON _ = mzero

{-|
  The ResultData data type corresponds to the nested resultData JSON object.
-}
data ResultData = ResultData {
  -- | The date of update.
  updatedDate :: String,
  -- | The total number of items found.
  totalItems :: Int,
  -- | The start index.
  startIndex :: Int,
  -- | The number of items to be shown per page.
  itemsPerPage :: Int,
  -- | A list of items (JSON objects).
  items :: [Item]
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the
  ResultData data type.
-}
instance FromJSON ResultData where
  parseJSON (Object v) = ResultData <$>
    v .: "updatedDate" <*>
    v .: "totalItems" <*>
    v .: "startIndex" <*>
    v .: "itemsPerPage" <*>
    parseJSON (fromJust $ HMS.lookup "items" v)
  parseJSON _ = mzero

{-|
  The Item data type corresponds to the nested list of items (JSON objects).
-}
data Item = Item {
  -- | The ID of the video.
  id :: String,
  -- | The date and time when the video was uploaded.
  uploaded :: String,
  -- | The date and time when the video was last updated.
  updated :: String,
  -- | The username of the uploader.
  uploader :: String,
  -- | The category of the video.
  category :: String,
  -- | The title of the video.
  title :: String,
  -- | The description of the video.
  description :: String,
  -- | A list of tags.
  tags :: [String],
  -- | A thumbnail JSON object.
  thumbnail :: Thumbnail,
  -- | A player JSON object.
  player :: Player,
  -- | A content JSON object.
  content :: Content,
  -- | The duration of the video in milliseconds.
  duration :: Int,
  -- | The aspect ratio of the video.
  aspectRatio :: String,
  -- | The rating of the video (0-5).
  rating :: Float,
  -- | The number of users who rated the video.
  ratingCount :: Int,
  -- | The number of times the video was viewed.
  viewCount :: Int,
  -- | The number of users who added the video to their favourites.
  favoriteCount :: Int,
  -- | The number of comments.
  commentCount :: Int,
  -- | A status JSON object.
  status :: Status,
  -- | An accessControl JSON object.
  accessControl :: AccessControl
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the Item
  data type.
-}
instance FromJSON Item where
  parseJSON (Object v) = Item <$>
    v .: "id" <*>
    v .: "uploaded" <*>
    v .: "updated" <*>
    v .: "uploader" <*>
    v .: "category" <*>
    v .: "title" <*>
    v .: "description" <*>
    v .: "tags" <*>
    parseJSON (fromJust $ HMS.lookup "thumbnail" v) <*>
    parseJSON (fromJust $ HMS.lookup "player" v) <*>
    parseJSON (fromJust $ HMS.lookup "content" v) <*>
    v .: "duration" <*>
    v .: "aspectRatio" <*>
    v .: "rating" <*>
    v .: "ratingCount" <*>
    v .: "viewCount" <*>
    v .: "favoriteCount" <*>
    v .: "commentCount" <*>
    parseJSON (fromJust $ HMS.lookup "status" v) <*>
    parseJSON (fromJust $ HMS.lookup "accessControl" v)
  parseJSON _ = mzero

{-|
  The Thumbnail data type corresponds to the nested thumbnail JSON objects.
-}
data Thumbnail = Thumbnail {
  -- | The thumbnail used for standard quality.
  sqDefault :: String,
  -- | The thumbnail used for high quality.
  hqDefault :: String
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the
  Thumbnail data type.
-}
instance FromJSON Thumbnail where
  parseJSON (Object v) = Thumbnail <$>
    v .: "sqDefault" <*>
    v .: "hqDefault"
  parseJSON _ = mzero

{-|
  The Player data type corresponds to the nested player JSON objects.
-}
data Player = Player {
  -- | The default player to be used.
  defaultPlayer :: String
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the Player
  data type.
-}
instance FromJSON Player where
  parseJSON (Object v) = Player <$>
    v .: "defaultPlayer"
  parseJSON _ = mzero

{-|
  The Content data type corresponds to the nested content JSON objects.
-}
data Content = Content {
  -- | Video content 1.
  content1 :: String,
  -- | Video content 5.
  content5 :: String,
  -- | Video content 6.
  content6 :: String
} deriving (Show)

{-|
    These functions are used to parse JSON objects that correspond to the
    Content data type.
-}
instance FromJSON Content where
  parseJSON (Object v) = Content <$>
    v .: "content1" <*>
    v .: "content5" <*>
    v .: "content6"
  parseJSON _ = mzero

{-|
  The Status data type corresponds to the nested status JSON objects.
-}
data Status = Status {
  -- | The value for audience restrictions.
  value :: String,
  -- | The reason for the value.
  reason :: String
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the Status
  data type.
-}
instance FromJSON Status where
  parseJSON (Object v) = Status <$>
    v .: "value" <*>
    v .: "reason"
  parseJSON _ = mzero

{-|
  The AccessControl data type corresponds to the nested accessControl JSON
  objects.
-}
data AccessControl = AccessControl {
  -- | Permission for syndication.
  syndicate :: Bool,
  -- | Permission for comment voting.
  commentVote :: Bool,
  -- | Permission for rating.
  rate :: Bool,
  -- | Permission for adding the video to lists.
  list :: Bool,
  -- | Permission for comments.
  comment :: Bool,
  -- | Permission for embedding the video.
  embed :: Bool,
  -- | Level of video respond.
  videoRespond :: String
} deriving (Show)

{-|
  These functions are used to parse JSON objects that correspond to the
  AccessControl data type.
-}
instance FromJSON AccessControl where
  parseJSON (Object v) = AccessControl <$>
    v .: "syndicate" <*>
    v .: "commentVote" <*>
    v .: "rate" <*>
    v .: "list" <*>
    v .: "comment" <*>
    v .: "embed" <*>
    v .: "videoRespond"
  parseJSON _ = mzero

{-|
  Main function that interacts with the user.
-}
main = do
  putStr "Enter name of file or full path: "
  hFlush stdout
  inputFile <- getLine
  inputFileExists <- doesFileExist inputFile
  if inputFileExists then do
    if (hasJSONExtension inputFile) then do
      contents <- BSL.readFile inputFile
      let dataset = decode contents :: Maybe Dataset
      if (validInput dataset) then
        promptOutput . translateToXML . fromJust $ dataset
      else
        handleError "input file is not valid for the data type defined"
    else
      handleError "input file is not a JSON file"
  else
    handleError "file not found"

{-|
  This functions gets a String with a file name and returns true if the file
  has a '.json' extension; false otherwise. It works even for files with upper
  case names, e.g. YOUTUBE.JSON.
-}
hasJSONExtension :: String -> Bool
hasJSONExtension file =
    (reverse (take 5 (reverse [toLower c | c <- file]))) == ".json"

{-|
  This function gets a Maybe Dataset and returns true if the input corresponds
  to a Dataset data type; false otherwise.
-}
validInput :: Maybe Dataset -> Bool
validInput Nothing = False
validInput (Just (Dataset _ _)) = True

{-|
  = __Part 2: Convert Haskell data type into XML format.__
-}

{-|
  This function asks the user to enter the name of the file where the XML
  translation will be saved. If the file already exists, it asks if the user
  wants to override it.
-}
promptOutput :: String -> IO ()
promptOutput content = do
  putStr "Enter name of output file: "
  hFlush stdout
  outputFile <- getLine
  outputFileExists <- doesFileExist outputFile
  if outputFileExists then
    overrideFile outputFile content
  else
    writeOutputFile outputFile content

{-|
  This function asks if the user wants to override a file in case it already
  exists. If the answer is no, the user will be prompted for a new file name.
-}
overrideFile :: String -> String -> IO ()
overrideFile file content = do
  putStrLn $ "The file " ++ file ++ " already exists."
  putStr $ "Do you wish to override it (yes/no)? "
  hFlush stdout
  answer <- getLine
  let override = [toLower c | c <- answer]
  case override of
    "no" -> promptOutput content
    "n" -> promptOutput content
    "yes" -> writeOutputFile file content
    "y" -> writeOutputFile file content
    _ -> overrideFile file content

{-|
  This function writes the XML translation to the specified file and notifies
  the user when this process is complete.
-}
writeOutputFile :: String -> String -> IO ()
writeOutputFile file content = do
  writeFile file content
  putStrLn "File successfully translated!"

{-|
  This function translates a Dataset into a String with the corresponding XML.
-}
translateToXML :: Dataset -> String
translateToXML dataset =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    (encodeDataset dataset tabs)
    where tabs = 0

{-|
  This function encodes a Dataset data type into XML.
-}
encodeDataset :: Dataset -> Int -> String
encodeDataset dataset tabs =
    "<dataset>\n" ++
    (addXMLElement "apiVersion" (apiVersion dataset) (tabs + 1)) ++ "\n" ++
    (encodeResultData (resultsData dataset) (tabs + 1)) ++
    "\n</dataset>"

{-|
  This function encodes a ResultData data type into XML.
-}
encodeResultData :: ResultData -> Int -> String
encodeResultData resultData tabs =
    (addTabs tabs) ++ "<resultsData>\n" ++
    (addXMLElements [
      ("updateDate", (updatedDate resultData)),
      ("totalItems", (show (totalItems resultData))),
      ("startIndex", (show (startIndex resultData))),
      ("itemsPerPage", (show (itemsPerPage resultData)))
    ] (tabs + 1)) ++ "\n" ++
    (encodeItemArray (items resultData) (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</resultsData>"

{-|
  This function encodes a list of Item data types into XML.
-}
encodeItemArray :: [Item] -> Int -> String
encodeItemArray [] _ = ""
encodeItemArray [x] tabs =
    (addTabs tabs) ++ "<items>\n" ++
    (encodeItem x (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</items>"
encodeItemArray (x:xs) tabs =
    (addTabs tabs) ++ "<items>\n" ++
    (encodeItem x (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</items>\n" ++
    (encodeItemArray xs tabs)

{-|
  This function encodes an Item data type into XML.
-}
encodeItem :: Item -> Int -> String
encodeItem item tabs =
    (addXMLElements [
      ("id", (Main.id item)),
      ("uploaded", (uploaded item)),
      ("uploader", (uploader item)),
      ("category", (category item)),
      ("title", (title item)),
      ("description", (description item))
    ] tabs) ++ "\n" ++
    (addXMLArray "tags" (tags item) tabs) ++ "\n" ++
    (encodeThumbnail (thumbnail item) tabs) ++ "\n" ++
    (encodePlayer (player item) tabs) ++ "\n" ++
    (encodeContent (content item) tabs) ++ "\n" ++
    (addXMLElements [
      ("duration", (show (duration item))),
      ("aspectRatio", (aspectRatio item)),
      ("rating", (show (rating item))),
      ("ratingCount", (show (ratingCount item))),
      ("viewCount", (show (viewCount item))),
      ("favoriteCount", (show (favoriteCount item))),
      ("commentCount", (show (commentCount item)))
    ] tabs) ++ "\n" ++
    (encodeStatus (status item) tabs) ++ "\n" ++
    (encodeAccessControl (accessControl item) tabs)

{-|
  This function encodes a Thumbnail data type into XML.
-}
encodeThumbnail :: Thumbnail -> Int -> String
encodeThumbnail thumbnail tabs =
    (addTabs tabs) ++ "<thumbnail>\n" ++
    (addXMLElements [
      ("sqDefault", (sqDefault thumbnail)),
      ("hqDefault", (hqDefault thumbnail))
    ] (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</thumbnail>"

{-|
  This function encodes a Player data type into XML.
-}
encodePlayer :: Player -> Int -> String
encodePlayer player tabs =
    (addTabs tabs) ++ "<player>\n" ++
    (addXMLElement "defaultPlayer" (defaultPlayer player) (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</player>"


{-|
  This function encodes a Content data type into XML.
-}
encodeContent :: Content -> Int -> String
encodeContent content tabs =
    (addTabs tabs) ++ "<content>\n" ++
    (addXMLElements [
      ("content1", (content1 content)),
      ("content5", (content5 content)),
      ("content6", (content6 content))
    ] (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</content>"

{-|
  This function encodes a Status data type into XML.
-}
encodeStatus :: Status -> Int -> String
encodeStatus status tabs =
    (addTabs tabs) ++ "<status>\n" ++
    (addXMLElements [
      ("value", (value status)),
      ("reason", (reason status))
    ] (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</status>"

{-|
  This function encodes an AccessControl data type into XML.
-}
encodeAccessControl :: AccessControl -> Int -> String
encodeAccessControl accessControl tabs =
    (addTabs tabs) ++ "<accessControl>\n" ++
    (addXMLElements [
      ("syndicate", (show (syndicate accessControl))),
      ("commentVoce", (show (commentVote accessControl))),
      ("list", (show (list accessControl))),
      ("comment", (show (comment accessControl))),
      ("embed", (show (embed accessControl))),
      ("videoRespond", (videoRespond accessControl))
    ] (tabs + 1)) ++ "\n" ++
    (addTabs tabs) ++ "</accessControl>"

{-|
  This recursive function gets a number and returns a String with that number of
  tabs.
-}
addTabs :: Int -> String
addTabs n
  | n < 1 = ""
  | n == 1 = "\t"
  | otherwise = "\t" ++ (addTabs (n - 1))

{-|
  This function encodes a generic XML element given two String containing a tag
  name and a value.
-}
addXMLElement :: String -> String -> Int -> String
addXMLElement tag value tabs =
    (addTabs tabs) ++ "<" ++ tag ++ ">" ++ value ++ "</" ++ tag ++ ">"

{-|
  This function encodes multiple generic XML elements given a list of pairs of
  Strings containing tag names and values.
-}
addXMLElements :: [(String, String)] -> Int -> String
addXMLElements [] _ = ""
addXMLElements [x] tabs = (addXMLElement (fst x) (snd x) tabs)
addXMLElements (x:xs) tabs =
    (addXMLElement (fst x) (snd x) tabs) ++ "\n" ++ (addXMLElements xs tabs)

{-|
  This function encodes a generic XML array given a list of Strings.
-}
addXMLArray :: String -> [String] -> Int -> String
addXMLArray _ [] _ = ""
addXMLArray tag [x] tabs = (addXMLElement tag x tabs)
addXMLArray tag (x:xs) tabs =
    (addXMLElement tag x tabs) ++ "\n" ++ (addXMLArray tag xs tabs)

{-|
  This function displays error messages on the screen.
-}
handleError :: String -> IO ()
handleError message = do
  putStrLn $ "Error: " ++ message ++ "!"
  main

{-|
  = __Part 3: Functions to query parsed data.__
-}

{-|
  This contains the JSON sample used for this project as a Haskell data type.
  It is only meant to be used for part 3 for testing the functions defined
  below.
-}
youtube = Dataset {
  apiVersion = "2.0",
  resultsData = ResultData {
    updatedDate = "2010-01-07T19:58:42.949Z",
    totalItems = 800,
    startIndex = 1,
    itemsPerPage = 1,
    items = [
      Item {
        Main.id = "hYB0mn5zh2c",
        uploaded = "2007-06-05T22:07:03.000Z",
        updated = "2010-01-07T13:26:50.000Z",
        uploader = "GoogleDeveloperDay",
        category = "News",
        title = "Google Developers Day US - Maps API Introduction",
        description = "Google Maps API Introduction ...",
        tags = ["GDD07","GDD07US","Maps"],
        thumbnail = Thumbnail {
          sqDefault = "http://i.ytimg.com/vi/hYB0mn5zh2c/default.jpg",
          hqDefault =
              "http://i.ytimg.com/vi/hYB0mn5zh2c/hqdefault.jpg"
        },
        player = Player {
          defaultPlayer =
              "http://www.youtube.com/watch?vu003dhYB0mn5zh2c"
        },
        content = Content {
          content1 = "rtsp://v5.cache3.c.youtube.com/CiILENy..." ++
              "/0/0/0/video.3gp",
          content5 = "http://www.youtube.com/v/hYB0mn5zh2c?f...",
          content6 = "rtsp://v1.cache1.c.youtube.com/CiILENy..." ++
              "/0/0/0/video.3gp"
        },
        duration = 2840,
        aspectRatio = "widescreen",
        rating = 4.63,
        ratingCount = 68,
        viewCount = 220101,
        favoriteCount = 201,
        commentCount = 22,
        status = Status {
          value = "restricted",
          reason = "limitedSyndication"
        },
        accessControl = AccessControl {
          syndicate = True,
          commentVote = True,
          rate = True,
          list = True,
          comment = True,
          embed = True,
          videoRespond = "moderated"
        }
      },
      Item {
        Main.id = "hxCxyYKQ1hM",
        uploaded = "2011-06-08T22:07:03.000Z",
        updated = "2012-03-05T13:26:50.000Z",
        uploader = "googlechromeuk",
        category = "Science & Technology",
        title = "Introducing the Chromebook",
        description = "Chromebooks are built and optimised for the " ++
            "web, so you get a faster, simpler and more secure " ++
            "experience without all the headaches of ordinary " ++
            "computers. Take a look: http://www.google.co.uk/" ++
            "chromebook",
        tags = ["Google","Chrome","Chromebook"],
        thumbnail = Thumbnail {
          sqDefault = "https://i.ytimg.com/vi_webp/hxCxyYKQ1hM/" ++
              "mqdefault.webp",
          hqDefault = "https://i.ytimg.com/vi_webp/hxCxyYKQ1hM/" ++
              "hqdefault.webp"
        },
        player = Player {
          defaultPlayer = "http://www.youtube.com/watch?" ++
              "vu003dhYB0mn5zh2c"
        },
        content = Content {
          content1 = "rtsp://v5.cache3.c.youtube.com/CiILENy..." ++
              "/0/0/0/video.3gp",
          content5 = "http://www.youtube.com/v/hYB0mn5zh2c?f...",
          content6 = "rtsp://v1.cache1.c.youtube.com/CiILENy..." ++
              "/0/0/0/video.3gp"
        },
        duration = 89400,
        aspectRatio = "widescreen",
        rating = 4.82,
        ratingCount = 287,
        viewCount = 45562,
        favoriteCount = 421,
        commentCount = 320,
        status = Status {
          value = "restricted",
          reason = "limitedSyndication"
        },
        accessControl = AccessControl {
          syndicate = True,
          commentVote = True,
          rate = True,
          list = True,
          comment = True,
          embed = True,
          videoRespond = "moderated"
        }
      }
    ]
  }
}

{-|
  This function returns a list with all the video titles.
-}
getItemsTitles :: Dataset -> [String]
getItemsTitles dataset = [(title i) | i <- (items . resultsData $ dataset)]

{-|
  This function returns the average duration of all the videos in milliseconds.
-}
getAverageDuration :: (Fractional a) => Dataset -> a
getAverageDuration dataset =
    (realToFrac (sum durations)) / (realToFrac (length durations))
      where durations = [(duration i) | i <- (items . resultsData $ dataset)]

{-|
  This function returns a list with a pair of all video titles and their
  corresponding rating.
-}
getTitlesRatings :: Dataset -> [(String, Float)]
getTitlesRatings dataset =
    [(title i, rating i) | i <- (items . resultsData $ dataset)]

{-|
  This function returns a list with a tuple containing the video ID, its
  category and a list of its tags for all the videos.
-}
getIDsCategoriesTags :: Dataset -> [(String, String, [String])]
getIDsCategoriesTags dataset =
    [(Main.id i, category i, tags i) | i <- (items . resultsData $ dataset)]

{-|
  This function returns a list with the video title and the percentage of users
  who commented it among all the user who added it to their favourites for all
  the videos.
-}
getPercentageComments :: Dataset -> [(String, Float)]
getPercentageComments dataset =
    [(title i, (percentage (commentCount i) (favoriteCount i)) * 100) |
    i <- (items . resultsData $ dataset)]
      where percentage c v = (realToFrac c) / (realToFrac v)
