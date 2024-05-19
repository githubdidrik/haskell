{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree



--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  let listOfWords = words contents
  let tree = foldr insert emptyTree listOfWords
  let treeSize      = size tree
  let treeHeight    = height tree
  let optimalHeight = ceiling (logBase 2 (fromIntegral (treeSize + 1))) - 1
  let heightRatio   = fromIntegral treeHeight / fromIntegral optimalHeight
  let correctTree   = checkTree tree

  putStrLn $ "Size: " ++ show treeSize
  putStrLn $ "Height: " ++ show treeHeight
  putStrLn $ "Optimal height: " ++ show optimalHeight
  putStrLn $ "Height / Optimal height: " ++ show heightRatio
  putStrLn $ "checkTree: " ++ show correctTree
  putStrLn $ "First 20 words: " ++ concatMap addSpace (take 20 (inorder tree))


addSpace :: String -> String
addSpace s = s ++ " "

--------------------------------------------------------------------------------

