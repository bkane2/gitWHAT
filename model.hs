module Model where

import Tree as G

-- The following can probably be refactored at some point (some are unused as well)
---------------------------------------------------------------------------------------------------------------
import Data.Hashable as H
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

-- Create NodeID by hashing on ByteString
hashNodeID :: ByteString -> NodeID
hashNodeID str = H.hash str

-- -- Creates an empty NodeIDTable
-- emptyNodeIDRecord :: NodeIDRecord
-- emptyNodeIDRecord = (0, Map.empty)

-- -- Inserts a pair of a ByteString key and a NodeID value into the table
-- insertNodeIDTable :: ByteString -> NodeID -> NodeIDTable -> NodeIDTable
-- insertNodeIDTable k v table = Map.insert k v table

-- -- Gets the NodeID at a certain ByteString key
-- getNodeIDTable :: ByteString -> NodeIDTable -> NodeID
-- getNodeIDTable k table = case (Map.lookup k table) of
--   Just a -> a
--   Nothing -> 0

-- -- If ByteString already has NodeID associated in table, return that, otherwise increment idx and store new NodeID
-- formNodeID :: ByteString -> NodeIDRecord -> (NodeID, NodeIDRecord)
-- formNodeID k (idx, table)
--   | intable == True  = ((getNodeIDTable k table), (idx, table))
--   | intable == False = (idx+1, (idx+1, (insertNodeIDTable k (idx+1) table)))
--   where intable = Map.member k table

-- -- NodeIDRecord is a hash map of ByteString to NodeID (aka NodeIDTable), together with a NodeID increment
-- type NodeIDTable = Map ByteString NodeID
-- type NodeIDRecord = (NodeID, NodeIDTable)

-- ByteString conversion functions
intToByteString :: Int -> ByteString
intToByteString i = strToByteString (show i)
strToByteString :: String -> ByteString
strToByteString str = C.pack str
---------------------------------------------------------------------------------------------------------------


-- Repository consists of a manifest (FileLog) and a list of FileLog for each file being versioned
type RepositoryID = Int
type Repository = (RepositoryID, FileLog, [FileLog], [Revision])
-- Revision is a unique RevisionID and a NodeID pointing to a particular version of the manifest FileLog
type RevisionID = Int
type Revision = (RevisionID, NodeID)
-- FileLog is a tree of FileVersion (each node is a FileVersion with two parents)
type FileID = Int
type FileName = String
type FileLog = (FileName, Tree FileVersion)
-- FileVersion is a unique NodeID together with ByteString contents
type NodeID = Int
type FileContents = ByteString
type FileVersion = (NodeID, FileContents)
