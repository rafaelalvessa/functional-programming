import Database.HDBC
import Database.HDBC.Sqlite3
import Network.HTTP hiding (close)
import Network.URI
import Data.Maybe
import Data.Either
import System.IO
import System.Directory
import Data.Text (pack, unpack, splitOn)

-- Database Exercises

data Record = Record {
  stock :: String,
  date :: String,
  open :: String,
  high :: String,
  low :: String,
  close :: String,
  volume :: Int,
  adj_close :: String
} deriving (Show)

-- Exercise 1

createTable :: IO ()
createTable = do
  conn <- connectSqlite3 "prices.db"
  run conn ("CREATE TABLE prices (stock VARCHAR(40) NOT NULL, " ++
      "date DATE NOT NULL, open VARCHAR(40) DEFAULT NULL, " ++
      "high VARCHAR(40) DEFAULT NULL, low VARCHAR(40) DEFAULT NULL, " ++
      "close VARCHAR(40) DEFAULT NULL, volume BIGINT(11) DEFAULT NULL, " ++
      "adj_close VARCHAR(40) DEFAULT NULL)") []
  commit conn
  disconnect conn

-- Exercise 4

readTable :: IO [String]
readTable = do
  content <- readFile "table.csv"
  let sample = take 100 (tail . lines $ content)
  return sample

-- Exercise 5

produceElement :: String -> Record
produceElement record = Record {
  stock = "YHOO",
  date = unpack $ elements !! 0,
  open = unpack $ elements !! 1,
  high = unpack $ elements !! 2,
  low = unpack $ elements !! 3,
  close = unpack $ elements !! 4,
  volume = read (unpack $ elements !! 5) :: Int,
  adj_close = unpack $ elements !! 6
} where elements = splitOn (pack ",") (pack record)

-- Exercise 6

insertRecord :: Record -> IO ()
insertRecord record = do
  conn <- connectSqlite3 "prices.db"
  stmt <- prepare conn "INSERT INTO prices VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  execute stmt [toSql . stock $ record, toSql . date $ record,
      toSql . open $ record, toSql . high $ record, toSql . low $ record,
      toSql . close $ record, toSql (volume record :: Int),
      toSql . adj_close $ record]
  commit conn
  disconnect conn

-- Exercise 7

uploadEntries :: IO ()
uploadEntries = do
  entries <- readTable
  let records = produceElements entries
  insertRecords records
  where produceElements [x] = [produceElement x]
        produceElements (x:xs) = [produceElement x] ++ (produceElements xs)
        insertRecords [x] = insertRecord x
        insertRecords (x:xs) = do
            insertRecord x
            insertRecords xs

-- HTTP Exercises

url :: String -> String
url stock = "http://real-chart.finance.yahoo.com/table.csv?s=" ++ stock

-- Exercise 1

downloadURL :: String -> IO (Either String String)
downloadURL url = do
  resp <- simpleHTTP request
  case resp of
    Left x -> return $ Left ("Error connecting: " ++ show x)
    Right r ->  case rspCode r of
      (2,_,_) -> return $ Right (rspBody r)
      (3,_,_) -> case findHeader HdrLocation r of
          Nothing -> return $ Left (show r)
          Just url -> downloadURL url
      _ -> return $ Left (show r)
    where request = Request {
      rqURI = uri,
      rqMethod = GET,
      rqHeaders = [],
      rqBody = ""
    }
          uri = fromJust $ parseURI url

{-- Exercise 2

putEntries :: String -> IO ()
putEntries stock = do
  fileExists <- doesFileExist "prices.db"
  if fileExists then do
    removeFile "prices.db"
    putEntries stock
  else do
    createTable
    content <- downloadURL . url $ stock
    let entries = tail . lines $ content
    let records = map produceElement entries
    map insertRecord records
    conn <- connectSqlite3 "prices.db"
    quickQuery conn "SELECT * FROM prices" []
    disconnect conn
-}
