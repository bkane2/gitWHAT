module FileLog where

import Model as M

-- Create an empty FileLog
createFileLog :: () -> FileLog
createFileLog () = Empty

-- add_version :: FileLog -> FileVersion -> FileLog
-- get_version :: FileLog -> Int -> FileVersion