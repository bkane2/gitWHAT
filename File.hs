module File where

import Model as M
import Util as U

import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

getFileContents :: File -> ByteString
getFileContents (fname, fcontents) = fcontents

getFileContentsFromList :: FileName -> [File] -> ByteString
getFileContentsFromList fname [] = BS.empty
getFileContentsFromList fname (x:xs) =
  let (fname1, fcontents) = x
  in if fname1 == fname then fcontents else (getFileContentsFromList fname xs)
