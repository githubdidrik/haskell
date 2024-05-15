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
import Data.List (foldl')
import Data.Ord (comparing)
--import Data.PSQueue





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
main = undefined  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath
 
























































































{- type DistanceMap a = Map a Int 


shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g start end = do
  dist <- dijkstra g start
  path <- getPath start end dist
  return (path, fromJust $ M.lookup end dist)


dijkstra :: (Ord a, Ord b) => Graph a b -> a -> Maybe (DistanceMap a)
dijkstra g start = dijkstra' (adjList g) (M.singleton start 0) (M.keysSet $ adjList g)


dijkstra' :: (Ord a, Ord b) => Map a [Edge a b] -> DistanceMap a -> S.Set a -> Maybe (DistanceMap a) 
dijkstra' adjList dists notVisited
  | M.null notVisited = Just dists
  | otherwise = do
    current <- closestVertex dists not
    let neighbors = M.findWithDefault [] current adjList 
    let newDists = relaxEdges current neighbors dists
    let newNotVisited = S.delete current notVisited
    dijkstra' adjList newDists newNotVisited

closestVertex :: Ord a => DistanceMap a -> S.Set a -> Maybe a 
closestVertex dists notVisited = S.lookupMin $ S.intersection notVisited (M.keysSet dists)

relaxEdges :: (Ord a, Num b) => a -> [Edge a b] -> DistanceMap a -> DistanceMap a
relaxEdges src edges dists = foldr  relax dists edges 
  where 
    relax (Edge src dst weight) ds = 
      let newDist = M.findWithDefault maxBound src ds + weight
      in M.insertWith min dst newDist ds -}
