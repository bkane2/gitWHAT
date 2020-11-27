module Repository where

import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Tree as T

-- -- Repository consists of a manifest (FileLog) and a list of FileLog for each file being versioned
-- type RepositoryID = String
-- type Repository = (RepositoryID, [Revision], FileLog, [FileLog])
-- -- Revision is a unique RevisionID and a NodeID pointing to a particular version of the manifest FileLog
-- type RevisionID = String
-- type Revision = (RevisionID, NodeID)
-- -- FileLog is a tree of FileVersion (each node is a FileVersion with two parents)
-- type FileID = Int
-- type FileName = String
-- type FileLog = (FileName, Tree FileVersion)
-- -- FileVersion is a unique NodeID together with ByteString contents
-- type NodeID = Int
-- type FileContents = ByteString
-- type FileVersion = (NodeID, FileContents)
-- -- File represents a file, i.e. FileName and FileContents
-- type File = (FileName, FileContents)

-- Create an empty repository
initRepository :: RepositoryID -> Repository
initRepository id = (id, [(RV.initRevision)], (FL.createFileLog ".manifest"), [])

-- Convert a repository to a string, showing log of revisions
printRepository :: Repository -> String
printRepository repo =
  let (repoID, revisions, manifest, logs) = repo
  in repoID ++ " ::\n" ++ (U.joinString "\n" (map RV.printRevision revisions))

-- Convert a repository to a string (printing FileLog information as well)
printRepositoryVerbose :: Repository -> String
printRepositoryVerbose repo =
  let (repoID, revisions, manifest, logs) = repo
  in (printRepository repo) ++ "\n" ++ createSep ++ (U.joinString createSep (map FL.printFileLog ([manifest]++logs)))
  

-- TODO:
-- store :: Repository -> Revision -> Repository
-- retrieve :: Repository -> Int -> Maybe Revision
