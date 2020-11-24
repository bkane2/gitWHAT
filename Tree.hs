module Tree where
import Data.Maybe (isNothing, fromJust)

-- A Tree is either Empty or a Branch of type a with two parent trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

-- Initializes an empty tree
initTree :: Tree a
initTree = Empty

-- Returns the specified node from the given tree, if it exists
getNode :: (Eq a) => a -> Tree a -> Maybe a
getNode _ Empty = Nothing
getNode n (Branch a b c) 
    | n == a = Just a
    | isNothing result = getNode n c 
    | otherwise = result
    where
        result = getNode n b

-- Returns entire trie of the specified node from the given tree
getTree :: (Eq a) => a -> Tree a -> Maybe (Tree a)
getTree _ Empty = Nothing
getTree n (Branch a b c) 
    | n == a = Just (Branch a b c)
    | isNothing result = getTree n c 
    | otherwise = result
    where
        result = getTree n b

commit :: a -> Tree a -> Tree a
commit n t = Branch n t Empty

-- Adds a given child, to the specified parent in the given tree
addOneParent :: (Eq a) => a -> a -> Tree a -> Maybe (Tree a)
addOneParent c p t = 
    if isNothing r1 then Nothing 
    else Just (Branch c (fromJust r1) Empty)
    where 
        r1 = getTree p t

-- Adds a given child, to the two specified parents in the given tree
addTwoParents :: (Eq a) => a -> a -> a -> Tree a -> Maybe (Tree a)
addTwoParents _ _ _ Empty = Nothing
addTwoParents c p1 p2 t = 
    if isNothing r1 || isNothing r2 then Nothing
    else Just (Branch c (fromJust r1) (fromJust r2))
    where
        r1 = getTree p1 t
        r2 = getTree p2 t
