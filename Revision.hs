module Revision where

import Model as M

-- TODO: Instead of [FileLog] here, this should probably just be a list of file names corresponding to the files being tracked (by the tracking list module)
-- revise :: Revision -> [FileLog] -> Revision