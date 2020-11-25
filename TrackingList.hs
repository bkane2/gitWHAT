module TrackingList where

import Model as M
import Data.List (delete)

-- TODO: add functionality here which lets filenames be added/subtracted from a tracking list, which is used
-- when creating a new revision (e.g. during a commit)

track :: [FileName] -> FileName -> [FileName]
track f_list f = if f `elem` f_list then f_list else f_list ++ [f] 

untrack :: [FileName] -> FileName -> [FileName]
untrack f_list f = delete f f_list