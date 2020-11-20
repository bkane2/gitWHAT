module FileVersion where

import Model as M

-- Create a new file version from a bytestring
-- TODO: if we're just maintaining a database of IDs somewhere and not using a SHA1 hash,
-- we can remove the 'hash' function here and probably get rid of the argument requirement
-- of the two parent nodes entirely.
createVersion :: FileContents -> (NodeID, NodeID) -> FileVersion
-- Inputs:
--  contents :: FileContents : a byte string of the file contents
--  (p1, p2) :: (NodeID, NodeID) : a tuple of two parent NodeIDs
-- Returns:
--  version :: FileVersion : a version with a NodeID created from hashing contents and parent NodeIDs
createVersion contents (p1, p2) =
  ((hash (contents <> (intToByteString p1) <> (intToByteString p2))), contents)