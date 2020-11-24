module TrackingList where

import Model as M
import Data.List (delete)

-- TODO: add functionality here which lets filenames be added/subtracted from a tracking list, which is used
-- when creating a new revision (e.g. during a commit)

-- TODO: these should probably just be lists of filenames (strings?) being tracked, rather than FileLog
track :: [String] -> String -> [String]
track f_list f = if f `elem` f_list then f_list else f_list ++ [f] 

untrack :: [String] -> String -> [String]
untrack f_list f = delete f f_list