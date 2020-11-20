module TrackingList where

import Model as M

-- TODO: add functionality here which lets filenames be added/subtracted from a tracking list, which is used
-- when creating a new revision (e.g. during a commit)

-- TODO: these should probably just be lists of filenames (strings?) being tracked, rather than FileLog
-- track :: [FileLog] -> FileLog -> [FileLog]
-- untrack :: [FileLog] -> FileLog -> [FileLog]