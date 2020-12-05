module Repository where

import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Tree as T

-- Create an empty repository
initRepository :: RepositoryID -> Repository
initRepository id = (id, [(RV.initRevision)], (FL.createFileLog ".manifest"), [])

-- Check if a repository has a given RevisionID
hasRevision :: RevisionID -> Repository -> Bool
hasRevision revId repo =
  let (_, revisions, _, _) = repo
  in elem revId (map fst revisions)
    
-- TODO:
-- store :: Repository -> Revision -> Repository
-- retrieve :: Repository -> Int -> Maybe Revision
