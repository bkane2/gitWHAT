module TrackingList where

import Model as M
import Util as U

-- Initiates an empty tracking list
emptyTrackingList :: [File]
emptyTrackingList = []

-- Loads a file from fname and adds file to the tracking list (if file already exists, read the most recent version)
track :: [File] -> FileName -> [File]
track f_list fname = if fname `elem` [ fname1 | (fname1, _) <- f_list ] then (track (untrack f_list fname) fname)
                     else f_list ++ [(fname, U.loadFile fname)]

-- Removes a file from the tracking list
untrack :: [File] -> FileName -> [File]
untrack f_list fname = filter (\l -> let (fname1, _) = l in fname1 /= fname) f_list