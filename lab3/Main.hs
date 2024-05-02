{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree
import Data.Char


--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  let listOfWords = parse contents []
  let tree = foldr insert emptyTree listOfWords
  let treeSize      = size tree
  let treeHeight    = height tree
  let optimalHeight = if treeSize <= 1 then 0 else ceiling (logBase 2 (fromIntegral (treeSize + 1))) - 1
  let heightRatio   = fromIntegral treeHeight / fromIntegral optimalHeight
  let correctTree   = checkTree tree

  putStrLn $ "Size: " ++ show treeSize
  putStrLn $ "Height: " ++ show treeHeight
  putStrLn $ "Optimal height: " ++ show optimalHeight
  putStrLn $ "Height / Optimal height: " ++ show heightRatio
  putStrLn $ "checkTree: " ++ show correctTree
  putStrLn $ "First 20 words: " ++ show (take 20 (inorder tree))


parse :: String -> String -> [String]
parse "" acc = [acc]
parse (c:cs) acc
  | isSpace c = acc : parse cs []
  | otherwise = parse cs (acc ++ [c])

--------------------------------------------------------------------------------

