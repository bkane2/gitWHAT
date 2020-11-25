module View where

import Model as M
import Tree as T

-- Given a Repository, prints the manifest file log
status :: Repository -> [IO ()]
status (repo_id, _, flog, _) = fmap putStrLn [show repo_id, printFileLog flog]
 where
  printFileLog flog = 
   let (_, tree) = flog in printTree tree