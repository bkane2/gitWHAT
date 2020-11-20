import Model as M
import FileLog as FL
import FileVersion as FV
import Revision as RV
import Repository as RP
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

main = do
  content <- BS.readFile "test/a.txt"
  print (FV.createVersion content (2, 3))