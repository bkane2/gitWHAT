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
  

-- TODO:
-- store :: Repository -> Revision -> Repository
-- retrieve :: Repository -> Int -> Maybe Revision
