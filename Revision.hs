module Revision where

import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- Creates an empty revision
initRevision :: Revision
initRevision = ("init", 0)

-- Takes a repository, previous revision (RevisionID + NodeID of manifest) and
-- a list of files, and returns a new repository with a new revision.
revise :: Repository -> RevisionID -> Revision -> [File] -> Repository
revise repo revId1 rev files =
  let (repoId, revs, man, logs) = repo
      (revId, manId) = rev
      fnames = [ a | (a,b) <- files ]
      -- Make sure all files in 'files' are logged, creating new FileLogs if not
      loggedFiles = map (\l -> let (a, _) = l in a) logs
      logs1 = map (\fname -> FL.fileLogLookup fname logs) fnames
      -- Get parent manifest version and contents
      manVersion = (FL.getVersion man manId)
      manContents = getVersionContents manVersion
      -- Translate manifest version to a map of (FileName, NodeID) pairs, adding new ones for newly tracked files
      manMap =
        let manMapExisting = nodeIDMapFromManifest manContents fnames
        in nodeIDListToMap [ (fname, 0) | fname <- fnames, notElem fname loggedFiles ] manMapExisting
      -- For each log in the revision set, create a new version and add to log given parent ID, recording new NodeID
      logsAndNodeIdsNew = map (\log ->
        let (fname, _) = log
            parentId = nodeIDFromMap fname manMap
            contents = M.getFileContentsFromList fname files
            versionNew = FV.createVersion contents (parentId, 0)
            nodeIdNew = FV.getVersionNodeID versionNew
            logNew = FL.addVersion log versionNew parentId Nothing
        in (logNew, (fname, nodeIdNew))) logs1
      logsNew = [ a | (a,b) <- logsAndNodeIdsNew ]
      manMapNew = [ b | (a,b) <- logsAndNodeIdsNew ]
      -- Create new manifest version with manId as parent NodeID and new nodeIds as
      manVersionNew = FV.createVersion (nodeIDListToContents manMapNew) (manId, 0)
      manIdNew = FV.getVersionNodeID manVersionNew
      manNew = FL.addVersion man manVersionNew manId Nothing
  -- Return updated repository
  in (repoId, ((revId1, manIdNew):revs), manNew, logsNew)


-- Given contents of manifest file, parse into a hash map of FileName to NodeID
-- manifest file contents are of the form "fname1 id1,fname2 id2,..."
-- Ignore files that are not in 'files'
nodeIDMapFromManifest :: FileContents -> [FileName] -> Map FileName NodeID
nodeIDMapFromManifest bstr files =
  let str = byteStringToStr bstr
      lst = map (\l -> let (fname:xs) = (words l)
                           (id:_) = xs
                       in (fname, U.strToInt id))
            (U.splitString str ',')
  in nodeIDListToMap [ (a,b) | (a,b) <- lst, elem a files ] Map.empty

-- Takes a list of (FileName, NodeID) tuples and hash map, and recursively fills in the hash map
nodeIDListToMap :: [(FileName, NodeID)] -> Map FileName NodeID -> Map FileName NodeID
nodeIDListToMap [] map = map
nodeIDListToMap (x:xs) map =
  let (fname, id) = x
  in Map.insert fname id (nodeIDListToMap xs map)

-- Gets a NodeID from a (FileName, NodeID) map, or returns 0 if it doesn't exist
nodeIDFromMap :: FileName -> Map FileName NodeID -> NodeID
nodeIDFromMap fname map = case (Map.lookup fname map) of
  Just a -> a
  Nothing -> 0

-- Given a list of (FileName, NodeID) tuples, convert to string of form "fname1 id1,fname2 id2,..."
nodeIDListToContents :: [(FileName, NodeID)] -> FileContents
nodeIDListToContents lst = U.strToByteString (U.joinString "," (map (\l -> let (a,b) = l in a++" "++(show b)) lst))


-- TODO:
-- merge :: Revision -> Revision -> Maybe Revision



