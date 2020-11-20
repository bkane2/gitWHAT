module Model where

-- import Graph as G 

---------------------------------------------------------------------------------------------------------------
-- import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

-- TODO: remove this line and uncomment Graph import statement above once Graph module finished
data Graph a = Empty | Node a (Graph a) (Graph a) deriving Show

-- TODO: maintain database of unique IDs rather than using SHA1
hash :: ByteString -> Int
hash str = 1

-- TODO: these should probably be moved into the software-decision module, as some sort of ByteString "utility" functions
intToByteString :: Int -> ByteString
intToByteString i = strToByteString (show i)
strToByteString :: String -> ByteString
strToByteString str = C.pack str
---------------------------------------------------------------------------------------------------------------
-- ABOVE PLACEHOLDERS SHOULD BE REMOVED WHEN OTHER MODULES ARE IN PLACE, OR MOVED INTO SEPARATE MODULES


-- Repository consists of a manifest (FileLog) and a list of FileLog for each file being versioned
type RepositoryID = Int
type Repository = (RepositoryID, FileLog, [FileLog])
-- Revision is a unique RevisionID and a NodeID pointing to a particular version of the manifest FileLog
type RevisionID = Int
type Revision = (RevisionID, NodeID)
-- FileLog is a graph of FileVersion (each node is a FileVersion with two parents)
type FileID = Int
type FileLog = Graph FileVersion
-- FileVersion is a unique NodeID together with ByteString contents
type NodeID = Int
type FileContents = ByteString
type FileVersion = (NodeID, FileContents)
