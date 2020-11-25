module CommandInterface where

import System.Environment
import Model as M
-- import Data.List.Split
import Tree as T
import Graph as G
import TrackingList as TL
import FileLog as FL
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
 | (input == "init") = 1
 | (input == "clone") = 2
 | (input == "add") = 2
 | (input == "remove") = 2
 | (input == "diff") = 2
 | (input == "status") = 1
 | (input == "cat") = 2
 | (input == "checkout") = 1
 | (input == "commit") = 2
 | otherwise = -1

--based on if valid command, if so then will take in next parameters
--based on command, must have testForValidityofCommand value as one input
testForValidParameters :: Int -> [String] -> String
testForValidParameters validity [input]  
 | validity < 0 = "Error in input. Review what you have typed and try again."
 | ([input] !! 0 == "clone") && validity == 2 = "Valid command! Processing..."
 | ([input] !! 0 == "add") && validity == 2 = "Valid command! Processing..."
 | ([input] !! 0 == "remove") && validity == 2 = "Valid command! Processing..."
 | ([input] !! 0 == "diff") && validity == 2 = "Valid command! Processing..."
 | ([input] !! 0 == "cat") && validity == 2 = "Valid command! Processing..."
 | ([input] !! 0 == "commit") && validity == 2 = "Valid command! Processing..."
 | ([input] !! 0 == "status") && validity == 1 = "Valid command! Processing..."
 | ([input] !! 0 == "checkout") && validity == 1 = "Valid command! Processing..."
 | ([input] !! 0 == "init") && validity == 0 = "Valid command! Processing..."
 | otherwise = "Invalid input parameters for the function you are using..."


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
initRepo = (1, [], createFileLog "flog", [])
 
-- Clones a repository given it's repository Id and the list of repositories it
-- is stored in, returns a new list of repositories with the clone
clone :: [Repository] -> RepositoryID -> Maybe [Repository]
clone [] _ = Nothing
clone repo_list id = search repo_list [] id where
   search repo_list visited id = case repo_list of 
      [] -> Nothing
      (repo_id, revs, flog, flog_list) : t -> if repo_id == id then  
          Just (visited ++ [(repo_id, revs, flog, flog_list)] ++ t ++ [(new_id + 1, revs, flog, flog_list)]) else
          search t (visited ++ [(repo_id, revs, flog, flog_list)]) id 
          where
            (new_id, _, _, _) = last t

-- Adds a list of files (second argument) to the given tracking list (first argument)
add :: [FileName] -> [FileName] -> [FileName]
add = foldl TL.track 

-- Removes a list of files (second argument) from the given tracking list (first
-- argument)
remove :: [FileName] -> [FileName] -> [FileName]
remove = foldl TL.untrack

-- Forwards 'status' command to view hiding module
-- status :: Repository -> [IO ()]
-- status = V.status

-- heads :: IO ()

-- diff :: Revision -> Revision -> String
-- diff revis1 revis2 = 
--  if revis1 RevisionID == revis2 RevisionID then "difference not detected"
--  else "difference detected"
-- -- cat :: FileID -> Revision -> 

-- -- checkout :: Revision -> 

-- commit :: Repository -> [FileID] -> Repository
-- commit

-- main :: IO ()
-- main = do
-- line <- getLine
-- let words = splitString line
-- let first = testForValidityOfCommand (words !! 0)
-- -- print (testForValidityOfCommand first)
-- print first
-- print (fmap testForValidityOfCommand words) --tests 

