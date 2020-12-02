import Model as M
import Util as U
import View as V
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import TrackingList as TL

import Data.Map (Map)
import qualified Data.Map as Map

main = do
  -- Track example files
  let trackingList = TL.emptyTrackingList
  let trackingList1 = TL.track trackingList "test/a.txt"
  let trackingList2 = TL.track trackingList1 "test/b.txt"
  let trackingList3 = TL.untrack trackingList2 "test/b.txt"
  print trackingList3

  -- Init empty repository
  let repo = RP.initRepository "testRepo"

  -- Get previous revision
  let (_, (lastRev:_), _, _) = repo

  -- Make new revision to repository (after tracking updates in file1 and file2)
  -- Requires previous revision to be provided as an argument
  let repo2 = RV.revise repo "test revision" lastRev trackingList3
  putStrLn ("\n"++(RP.printRepositoryVerbose repo2))