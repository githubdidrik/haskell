{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Route
import RouteGUI
import Graph
import Skew
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import System.Environment (getArgs)

------------------------------------------------------------

graph1 = addEdge "A" "D" 100 $ addEdge "C" "D" 1 $ addEdge "B" "C" 3 $ addEdge "A" "B" 5 $ addVertices ["A", "B", "C", "D"] empty
graph2 = addEdge "A" "C" 8 $ addEdge "B" "C" 2 $ addEdge "A" "B" 5 $ addEdge "A" "B" 10 $ addEdge "C" "D" 2 $ addEdge "A" "D" 5 $  addEdge "D" "E" 5 $ addVertices ["A", "B", "C", "D", "E"] empty
graph3 = addEdge "A" "B" 5 $ addEdge "A" "B" 10 $ addVertices ["A", "B", "C"] empty

initQueue :: (Ord a, Ord b, Num b, Ord b) => a -> SkewHeap (b, a, [a])
initQueue start = singleton (0, start, [])

shortestPath :: (Ord a, Ord b, Num b, Ord b) => Graph a b ->  a -> a -> Maybe ([a], b)
shortestPath graph start end = dijkstra (initQueue start) M.empty
  where
    dijkstra queue visited
      | isEmpty queue            = Nothing
      | current == end           = Just (reverse (current:path), dist)
      | M.member current visited = dijkstra restOfQueue visited
      | otherwise                = dijkstra newQueue newVisited
      where
        closest               = fromJust (root queue)
        (dist, current, path) = closest
        restOfQueue           = delete closest queue
        neighbours            = [(dist + label e, dst e, current:path) | e <- adj current graph, M.notMember (dst e) visited]
        newQueue              = foldr insert restOfQueue neighbours
        newVisited            = M.insert current dist visited

main :: IO ()
main = do  -- TODO: read arguments, build graph, output shortest path
  args <- getArgs
  let [stopsFile, linesFile, start, end] = args
  Right stops <- readStops stopsFile
  Right lines <- readLines linesFile

  let graph = buildGraph stops lines
      shortest = case shortestPath graph start end of
        Just (stops, dist) -> unwords stops ++ " with cost " ++ show dist
        Nothing            -> "aint no way"
  putStrLn shortest

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-gbg.txt"
  Right lines <- readLines "lines-gbg.txt"
  let graph = buildGraph stops lines
  print "slut"
  runGUI stops lines graph shortestPath

buildGraph :: [Stop] -> [LineTable] -> Graph String Integer
buildGraph stoops lineTables = addLines lineTables (addVertices [name s | s <- stoops] empty)
  where
    addLines [] g = g
    addLines (lineTable : rest) g = addLines rest $ addLine (stops lineTable) g
      where
        addLine (ls1 : ls2 : lss) g = addLine (ls2 : lss) (addBiEdge (stopName ls1) (stopName ls2) (time ls2) g)
        addLine _ g = g

