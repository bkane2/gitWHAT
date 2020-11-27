module Util where

import Data.Hashable as H
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List as L

-- Create NodeID by hashing on ByteString
hashNodeID :: ByteString -> Int
hashNodeID str = H.hash str

-- Conversion functions
strToInt :: String -> Int
strToInt str = read str :: Int
intToByteString :: Int -> ByteString
intToByteString i = strToByteString (show i)
strToByteString :: String -> ByteString
strToByteString str = C.pack str
byteStringToStr :: ByteString -> String
byteStringToStr str = C.unpack str
emptyByteString :: ByteString
emptyByteString = BS.empty

-- Split string by comma
splitString :: String -> Char -> [String]
splitString [] _ = []
splitString (c:cs) chr
  | c == chr = "" : rest
  | otherwise = (c : Prelude.head rest) : Prelude.tail rest
  where rest = splitString cs chr

-- Join a list of strings by some string
joinString :: String -> [String] -> String
joinString _ [] = ""
joinString str lst = (L.intercalate str lst)

-- File IO (wrapper function for loading file)
loadFile :: String -> IO ByteString
loadFile fname = BS.readFile fname

-- Creates a separator string
createSep :: String
createSep = "---------------------------------\n"