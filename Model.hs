module Model where

import Tree as T
import Tree (Tree)

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

-- ByteString conversion functions
intToByteString :: Int -> ByteString
intToByteString i = strToByteString (show i)
strToByteString :: String -> ByteString
strToByteString str = C.pack str
---------------------------------------------------------------------------------------------------------------


-- Repository consists of a manifest (FileLog) and a list of FileLog for each file being versioned
type RepositoryID = Int
type Repository = (RepositoryID, [Revision], FileLog, [FileLog])
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
