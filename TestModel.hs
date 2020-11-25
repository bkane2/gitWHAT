import Model as M
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.Map (Map)
import qualified Data.Map as Map

main = do
  -- TODO: clean up
  content <- BS.readFile "test/a.txt"
  let file1 = ("test/a.txt", content)
  let repo = RP.initRepository "testRepo"
  print (FV.createVersion content (-1154686932835703469, 0))
  print repo
  let (_, (lastRev:_), _, _) = repo
  print lastRev
  -- let repo2 = RV.revise repo "test revision" lastRev ["test/a.txt"]
  let repo2 = RV.revise repo "test revision" lastRev [file1]
  print repo2
  -- print (nodeIDListToContents [("fname1", 0), ("fname2", 1)])
  -- 
  contentMan <- BS.readFile "test/manifest_test.txt"
  let manMap = RV.nodeIDMapFromManifest contentMan ["fname1", "fname2"]
  print (Map.keys manMap)
