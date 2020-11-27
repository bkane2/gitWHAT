import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.Map (Map)
import qualified Data.Map as Map

main = do
  -- Load example files
  content1 <- BS.readFile "test/a.txt"
  content2 <- BS.readFile "test/b.txt"
  let file1 = ("test/a.txt", content1)
  let file2 = ("test/b.txt", content2)

  -- Init empty repository
  let repo = RP.initRepository "testRepo"

  -- Get previous revision
  let (_, (lastRev:_), _, _) = repo

  -- Make new revision to repository (after tracking updates in file1 and file2)
  -- Requires previous revision to be provided as an argument
  let repo2 = RV.revise repo "test revision" lastRev [file1, file2]
  print repo2