import System.IO (stdout, hFlush)
import System.Directory.Internal.Prelude (unless)
import CommandInterface as CI
import Model as M

-- Reads in a new command
read_ :: IO String
read_ = putStr "gitWHAT?> "
    >> hFlush stdout
    >> getLine

-- Evaluates command and passes along repository list
eval_ :: String -> [Repository] -> (String, [Repository])
eval_ "init" repos = ("Initializing new repository...", repos ++ [initRepo])
eval_ _ [] = ("First command must be init.", [])
eval_ input repos = (input, repos)
-- eval_ _ repos = ("I did not understand that command.", repos)

-- Prints informative message
print_ :: String -> IO ()
print_ = putStrLn

-- Driver function of the REPL loop
run :: [Repository] -> IO ()
run repos = do
    input <- read_
    
    unless (input == ":quit") $
        let evalInput = eval_ input repos in
        print_ (fst evalInput) >> run (snd evalInput)

-- Main function
main :: IO ()
main = do 
    putStrLn "Enter :quit at anytime to exit."
    run []
    
    