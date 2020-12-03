module CommandInterface where

import System.Environment
import Text.Read

import Model as M
import Tree as T
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import TrackingList as TL
import qualified View as V

-- Parses input into command and list of additional parameters, based on spaces in input
parseInput :: String -> (String, [String])
parseInput input =
   let words = splitString input
   in (head words, tail words)

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

-- Ensures that target repository exists in repoStates
repoExists :: String -> [RepositoryState] -> Bool
repoExists repo repoStates =
   let repoIds = [ repoId | ((repoId, _, _, _), _, _) <- repoStates ]
   in elem repo repoIds

-- Applies a command to a particular RepositoryState, returning a new list of RepositoryState
-- with the modified version.
applyToRepoState :: (RepositoryState -> RepositoryState) -> String -> [RepositoryState] -> [RepositoryState]
applyToRepoState _ _ [] = []
applyToRepoState f repo (x:xs) =
   let ((repoId, _, _, _), _, _) = x
   in if repo == repoId then ((f x):xs)
      else (x:(applyToRepoState f repo xs))

--tests to see if correct command is inputted, if so
--returns true and then will be used for "testForValidParameters"
--function if returned true
-- testForValidityOfCommand :: String -> Int
-- testForValidityOfCommand input
--  | (input == " ") = -1
--  | (input == "init") = 1
--  | (input == "clone") = 2
--  | (input == "add") = 2
--  | (input == "remove") = 2
--  | (input == "diff") = 2
--  | (input == "status") = 1
--  | (input == "cat") = 2
--  | (input == "checkout") = 1
--  | (input == "commit") = 2
--  | otherwise = -1

-- Tests to see if the parameters for a given command are correct and valid.
-- NOTE: if command is not supported at all, this is caught elsewhere.
testForValidityOfCommand :: String -> [String] -> [RepositoryState] -> (Bool, String)
testForValidityOfCommand command params repoStates
   -- init must have 1 param, and it must be a repoID that doesn't exist yet
   | (command == "init") && ((length params) /= 1) = (False, "init must be called as 'init <repoName>'.")
   | (command == "init") && (repoExists (params !! 0) repoStates) = (False, (params !! 0)++" already exists.")
   | otherwise = (True, "")

-- --based on if valid command, if so then will take in next parameters
-- --based on command, must have testForValidityofCommand value as one input
-- testForValidParameters :: Int -> [String] -> String
-- testForValidParameters validity [input]  
--  | validity < 0 = "Error in input. Review what you have typed and try again."
--  | ([input] !! 0 == "clone") && validity == 2 = "Valid command! Processing..."
--  | ([input] !! 0 == "add") && validity == 2 = "Valid command! Processing..."
--  | ([input] !! 0 == "remove") && validity == 2 = "Valid command! Processing..."
--  | ([input] !! 0 == "diff") && validity == 2 = "Valid command! Processing..."
--  | ([input] !! 0 == "cat") && validity == 2 = "Valid command! Processing..."
--  | ([input] !! 0 == "commit") && validity == 2 = "Valid command! Processing..."
--  | ([input] !! 0 == "status") && validity == 1 = "Valid command! Processing..."
--  | ([input] !! 0 == "checkout") && validity == 1 = "Valid command! Processing..."
--  | ([input] !! 0 == "init") && validity == 0 = "Valid command! Processing..."
--  | otherwise = "Invalid input parameters for the function you are using..."


-- Evaluates a command with a list of params, returning an informative message (either an
-- error or a view of the repository for a given command) and updated repository states
evalCommand :: String -> [String] -> [RepositoryState] -> (String, [RepositoryState])
evalCommand command params repoStates =
   let (validity, errorMsg) = testForValidityOfCommand command params repoStates
   in if validity == False then (errorMsg, repoStates)
      else executeCommand command params repoStates

-- Executes a command once validity of command has been verified. Returns an informative
-- message and updated repository states
executeCommand :: String -> [String] -> [RepositoryState] -> (String, [RepositoryState])
executeCommand "init" params repoStates = ("Initializing new repository...", initRepo (params !! 0) repoStates)
executeCommand _ _ [] = ("First command must be 'init <repoName>'.", [])
executeCommand "repos" _ repoStates = (printRepos repoStates, repoStates)
executeCommand "clone" params repoStates = ("TBC", repoStates)
executeCommand "add" params repoStates = ("TBC", repoStates)
executeCommand "remove" params repoStates = ("TBC", repoStates)
executeCommand "status" params repoStates = ("TBC", repoStates)
executeCommand "diff" params repoStates = ("TBC", repoStates)
executeCommand "cat" params repoStates = ("TBC", repoStates)
executeCommand "checkout" params repoStates = ("TBC", repoStates)
executeCommand "commit" params repoStates = ("TBC", repoStates)
executeCommand "log" params repoStates = ("TBC", repoStates)
executeCommand "merge" params repoStates = ("TBC", repoStates)
executeCommand "pull" params repoStates = ("TBC", repoStates)
executeCommand "push" params repoStates = ("TBC", repoStates)
executeCommand _ _ repoStates = ("I did not understand that command.", repoStates)

------------------------------------------------------------------------------------
-- FUNCTIONS FOR COMMANDS DEFINED BELOW
------------------------------------------------------------------------------------

-- Initializes an empty repository
initRepo :: RepositoryID -> [RepositoryState] -> [RepositoryState]
initRepo name repoStates =
   let newRepo = RP.initRepository name
       (_, revs, _, _) = newRepo
       hdRevId = fst (head revs)
       newRepoState = (newRepo, hdRevId, [])
   in newRepoState : repoStates

-- Prints the IDs of all repositories in repoStates
printRepos :: [RepositoryState] -> String
printRepos [] = ""
printRepos (x:xs) =
   let ((repoID, _, _, _), _, _) = x
   in "* " ++ repoID ++ "\n" ++ (printRepos xs)
 
-- Clones a repository given it's repository Id and the list of repositories it
-- is stored in, returns a new list of repositories with the clone
-- TODO: update so RepositoryState is used rather than Repository
-- clone :: [Repository] -> RepositoryID -> Maybe [Repository]
-- clone [] _ = Nothing
-- clone repo_list id = search repo_list [] id where
--    search repo_list visited id = case repo_list of 
--       [] -> Nothing
--       (repo_id, revs, flog, flog_list) : t -> if repo_id == id then  
--           Just (visited ++ [(repo_id, revs, flog, flog_list)] ++ t ++ [(new_id + 1, revs, flog, flog_list)]) else
--           search t (visited ++ [(repo_id, revs, flog, flog_list)]) id 
--           where
--             (new_id, _, _, _) = last t

-- Adds a list of files (second argument) to the given tracking list (first argument)
add :: [File] -> [FileName] -> [File]
add = foldl TL.track 

-- Removes a list of files (second argument) from the given tracking list (first
-- argument)
remove :: [File] -> [FileName] -> [File]
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