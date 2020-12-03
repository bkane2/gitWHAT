module CommandInterface where

import System.Environment
import Text.Read

import Model as M
import Util as U
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
   let words = U.splitString input ' '
   in (head words, tail words)

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

-- Retrieves a particular RepositoryState with a given name.
getRepoState :: String -> [RepositoryState] -> RepositoryState
getRepoState _ (x:[]) = x
getRepoState repo (x:xs) =
   let ((repoId, _, _, _), _, _) = x
   in if repo == repoId then x else (getRepoState repo xs)

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
   -- init <repoName>
   | (command == "init") && ((length params) /= 1) = (False, "init must be called as 'init <repoName>'.")
   | (command == "init") && (repoExists (params !! 0) repoStates) = (False, (params !! 0)++" already exists.")
   -- clone <newRepoName> <oldRepoName>
   -- TBC
   -- add <repoName> <filePath1> [filePath2] [filePath3] ...
   | (command == "add") && ((length params) /= 2) = (False, "add must be called as 'add <repoName> <filePath1> [filePath2] [filePath3] ...'")
   | (command == "add") && (not (repoExists (params !! 0) repoStates)) = (False, "repo "++(params !! 0)++" does not exist.")
   | (command == "add") && (not (all U.fileExists (tail params))) = (False, "File does not exist. Make sure paths are correct.")
   -- remove <repoName> <filePath1> [filePath2] [filePath3] ...
   | (command == "remove") && ((length params) /= 2) = (False, "remove must be called as 'remove <repoName> <filePath1> [filePath2] [filePath3] ...'")
   | (command == "remove") && (not (repoExists (params !! 0) repoStates)) = (False, "repo "++(params !! 0)++" does not exist.")
   -- status <repoName>
   | (command == "status") && (((length params) < 1) || ((length params) > 2)) = (False, "status must be called as 'remove <repoName> [-v]'")
   | (command == "status") && ((length params) == 2) && ((params !! 1) /= "-v") = (False, "status must be called as 'remove <repoName> [-v]'")
   | (command == "status") && (not (repoExists (params !! 0) repoStates)) = (False, "repo "++(params !! 0)++" does not exist.")
   -- diff <repoName> <revID1> <revID2>
   -- TBC
   -- cat <repoName> <revID> <filePath>
   -- TBC
   -- checkout <repoName> <revID>
   -- TBC
   -- commit <repoName> <revID>
   -- TBC
   -- log <repoName> [-v]
   -- TBC
   -- merge <repoName> <revID1> <revID2>
   -- TBC
   -- pull <repoName1> <repoName2>
   -- TBC
   -- push <repoName1> <repoName2>
   -- TBC
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
executeCommand "add" params repoStates = ("Tracking list updated.", (applyToRepoState (add (tail params)) (params !! 0) repoStates))
executeCommand "remove" params repoStates = ("Tracking list updated.", (applyToRepoState (remove (tail params)) (params !! 0) repoStates))
executeCommand "status" params repoStates = ((status (getRepoState (params !! 0) repoStates) (tail params)), repoStates)
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
   in (newRepo, (head revs), []) : repoStates

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

-- Adds a list of files to the tracking list, given a particular repository state
add :: [FileName] -> RepositoryState -> RepositoryState
add fnames repoState =
   let (repo, head, trackingList) = repoState
   in (repo, head, foldl TL.track trackingList fnames)

-- Removes a list of files from the tracking list, given a particular repository state
remove :: [FileName] -> RepositoryState -> RepositoryState
remove fnames repoState =
   let (repo, head, trackingList) = repoState
   in (repo, head, foldl TL.untrack trackingList fnames)

-- Gets status of tracking list given a repository state (and possibly a -v flag for verbose)
status :: RepositoryState -> [String] -> String
status repoState flags = 
   let (repo, head, trackingList) = repoState
   in if flags == []
      then V.printTrackingList trackingList
      else V.printTrackingListVerbose trackingList

-- heads :: IO ()

-- diff :: Revision -> Revision -> String
-- diff revis1 revis2 = 
--  if revis1 RevisionID == revis2 RevisionID then "difference not detected"
--  else "difference detected"
-- -- cat :: FileID -> Revision -> 

-- -- checkout :: Revision -> 

-- commit :: Repository -> [FileID] -> Repository
-- commit