import Model as M
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

main = do
  content <- BS.readFile "test/a.txt"
  print (FV.createVersion content (-1154686932835703469, 0))

  -- FL.createFileLog
  -- print (show filelog)