-- import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

data Graph a = Empty | Node a (Graph a) (Graph a) deriving Show
hash :: ByteString -> Int
hash str = 1
-- ABOVE PLACEHOLDERS SHOULD BE REMOVED WHEN OTHER MODULES ARE IN PLACE


type RevisionID = Int
type Revision = (RevisionID, [FileLog]) -- Revision has a Revision ID (cf. Changeset ID), and a list of FileLog (cf. Revlog)
type FileID = Int
type FileLog = (FileID, Graph FileVersion) -- FileLog has a FileID, and a graph of FileVersions (each version has a pointer to two parents)
type NodeID = Int
type FileVersion = (NodeID, ByteString) -- FileVersion has a NodeID, and a string of file contents


intToByteString :: Int -> ByteString
intToByteString i = strToByteString (show i)
strToByteString :: String -> ByteString
strToByteString str = C.pack str


createVersion :: ByteString -> (NodeID, NodeID) -> FileVersion
-- Inputs:
--  contents :: ByteString : a byte string of the file contents
--  (p1, p2) :: (NodeID, NodeID) : a tuple of two parent NodeIDs
-- Returns:
--  version :: FileVersion : a version with a NodeID created from hashing contents and parent NodeIDs
createVersion contents (p1, p2) =
  ((hash (contents <> (intToByteString p1) <> (intToByteString p2))), contents)


main = do
  content <- BS.readFile "test/a.txt"
  print (content <> content <> content)
  print ("test1" ++ ("test2" ++ "test3"))
  print (createVersion content (2, 3))