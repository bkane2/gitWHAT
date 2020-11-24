module CommandInterface where

import System.Environment
import Model as M
-- import Data.List.Split
import Tree as T
import Graph as G
import TrackingList as TL
import qualified View as V

splitString :: String -> [String]
--dropWhile will create a list from another when all parameters met are true.
--this set is a space char for this case. this will then feed the resulting array to splitWords
--which is a helper function within splitString under the where
splitString = splitWords . dropWhile (==' ') 
 where
  splitWords "" = [] --this is the base case of an empty string
  splitWords string = let --creates string
   singleWord = takeWhile (/=' ') string --a single word is taken before an instance of a space from the string
   (_, xs) = splitAt (length singleWord) string --splits string at the length of the first word and leaves the rest
   --recursive statement for rest ... dropWhile returns suffix of takeWhile so here returns
   --the remaining from after the space to send back recursively to finish getting each string
   in singleWord : splitWords (dropWhile (==' ') xs)

--tests to see if correct command is inputted, if so
--returns true and then will be used for "testForValidParameters"
--function if returned true
testForValidityOfCommand :: String -> Int
testForValidityOfCommand input
 | (input == " ") = -1
 | (input == "push") = 0
 | otherwise = -1

-- executeCommand :: [String] -> Either (IO ()) a
-- executeCommand ["init"] = Right initRepo
-- executeCommand ["clone", repo] = Right (clone repo)
-- executeCommand ("add" : files) = foldl track files
-- executeCommand ("remove" : files) = foldl untrack files
-- executeCommand ["status"] = status
-- executeCommand ["heads"] = heads
-- executeCommand ["diff", rev1, rev2] = diff rev1 rev2
-- executeCommand ["cat", file, rev] = cat file rev
-- executeCommand ["checkout", rev] = checkout rev
-- executeCommand ["commit"] = commit
-- executeCommand _ = Left (print "I don't understand that command.")

-- Initializes an empty repository
initRepo :: M.Repository
initRepo = (1, T.initTree, [])
 
-- Clones a repository given it's repository Id and the list of repositories it
-- is stored in, returns a new list of repositories with the clone
clone :: [Repository] -> RepositoryID -> Maybe [Repository]
clone [] _ = Nothing
clone repo_list id = search repo_list [] id where
   search repo_list visited id = case repo_list of 
      [] -> Nothing
      (repo_id, flog, flog_list) : t -> if repo_id == id then  
          Just (visited ++ [(repo_id, flog, flog_list)] ++ t ++ [(new_id + 1, flog, flog_list)]) else
          search t (visited ++ [(repo_id, flog, flog_list)]) id 
          where
            (new_id, _, _) = last t

-- Adds a list of files (second argument) to the given tracking list (first argument)
add :: [FileID] -> [FileID] -> [FileID]
add = foldl TL.track 

-- Removes a list of files (second argument) from the given tracking list (first
-- argument)
remove :: [FileID] -> [FileID] -> [FileID]
remove = foldl TL.untrack

-- Forwards 'status' command to view hiding module
status :: Repository -> [IO ()]
status = V.status

-- heads :: IO ()

-- diff :: Revision -> Revision -> 

-- cat :: FileID -> Revision -> 

-- checkout :: Revision -> 

-- commit :: () -> 

--based on if valid command, if so then will take in next parameters
--based on command, 
-- testForValidParameters :: Int -> [String] -> String

-- printString :: [String] -> String
-- printString [] = return ()
-- printString (x:xs) = do
--  putStrLn x
--  printString xs