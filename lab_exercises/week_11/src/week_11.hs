{-# LANGUAGE DeriveDataTypeable #-}

import Data.Word
import Data.Either
import Data.Maybe
import Data.Char
import Data.Typeable
import qualified Data.ByteString.Lazy as L
import Language.Haskell.TH.Ppr (bytesToString)
import qualified Control.Exception as E
import Network.HTTP.Conduit
import System.IO
import System.IO.Error

{- USING CUSTOM TYPES FOR DEALING WITH ERROR -}

data Result = DivByZero | OneIsBoring | Result Double
              deriving (Show)

safeInv :: Double -> Result
safeInv 0 = DivByZero
safeInv 1 = OneIsBoring
safeInv n = Result (1/n)

{- DEFINING YOUR OWN EXCEPTIONS -}

data MyErrors = NumberTooBig | NumberTooSmall deriving (Show, Typeable)

instance E.Exception MyErrors

goodNum :: Int -> Bool
goodNum n | n < 5 = E.throw NumberTooBig
          | n > 10 = E.throw NumberTooSmall
          | otherwise = True

{- USING EXCEPTIONS WHEN READING FILES -}

main1 :: IO ()
main1 = do
  input <- tryIOError (readFile "input.txt")
  case input of
    Left e -> print "Oh no, it seems file doesn't exist!"
    Right x -> print x

{-
 - USING EXCEPTIONS WHEN READING FILES
 - CHECKING THE TYPE OF EXCEPTION
-}

f :: IOError -> IO String
f e | ioeGetErrorType e == doesNotExistErrorType = return "File doesn't exist"
    | otherwise = return "Some other error"

main2 :: IO ()
main2 = do
  input <- catchIOError (readFile "input.txt") f
  print input

{- DOWNLOAD USING BYTESTRING -}

type HTTP = String
type Text = String

downloadPage :: HTTP -> IO Text
downloadPage http = do
  result <- E.try (simpleHttp http) :: IO (Either HttpException L.ByteString)
  let e = lefts [result]
  if length e == 0 then do
    let text_bs = head.rights $ [result]
    let html_word8 = L.unpack text_bs  :: [Word8]
    return $ bytesToString html_word8  :: IO Text
  else
    return ""
