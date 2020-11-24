import Tree as T 
import View as V

tree = Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 1 Empty Empty)

main = do
    putStrLn $ T.printTree tree