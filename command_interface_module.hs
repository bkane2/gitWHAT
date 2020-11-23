import System.Environment
-- import Data.List.Split

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


-- printString :: [String] -> String
-- printString [] = return ()
-- printString (x:xs) = do
--  putStrLn x
--  printString xs

main :: IO ()
main = do
line <- getLine
let words = splitString line
fmap print words --tests so far to print out each string

testForValidity :: String -> Bool
testForValidity = ""

