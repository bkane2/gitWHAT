module Util where

import Data.Hashable as H
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

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
splitComma :: String -> [String]
splitComma [] = []
splitComma (c:cs)
  | c == ','  = "" : rest
  | otherwise = (c : Prelude.head rest) : Prelude.tail rest
  where rest = splitComma cs