{-# LANGUAGE BlockArguments #-}
module Main where

import Route
import RouteGUI
import Graph
import Skew
import qualified Data.Map as M
import Data.Map (Map)
--import Data.List (singleton)
import qualified Data.Set as S
import Data.Maybe
import Control.Monad (when)




------------------------------------------------------------

graph1 = addEdge "A" "D" 100 $ addEdge "C" "D" 1 $ addEdge "B" "C" 3 $ addEdge "A" "B" 5 $ addVertices ["A", "B", "C", "D"] empty
graph2 = addEdge "A" "C" 8 $ addEdge "B" "C" 2 $ addEdge "A" "B" 5 $ addEdge "A" "B" 10 $ addEdge "C" "D" 2 $ addEdge "A" "D" 5 $  addEdge "D" "E" 5 $ addVertices ["A", "B", "C", "D", "E"] empty
graph3 = addEdge "A" "B" 5 $ addEdge "A" "B" 10 $ addVertices ["A", "B", "C"] empty


shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g start end = if end `elem` stops then Just (stops, distance) else Nothing
    where
      initSet   = M.empty
      initQueue = singleton (Edge start start 0)
      verts     = vertices g
      (s, q)    = dijkstraLoop g initSet initQueue verts
      stops = map (\(Edge stop _ _) -> stop) (M.keys s)
      distance  = maximum (map (\(Edge _ _ dist) -> dist) (M.keys s))
      --distance  = fromMaybe 0 (M.lookup end s)

-- loop through all vertices to find shortest path
dijkstraLoop :: (Ord a, Ord b, Num b) => Graph a b -> Map (Edge a b) b -> SkewHeap (Edge a b) -> [a] -> (Map (Edge a b) b, SkewHeap (Edge a b))
dijkstraLoop g s q []       = (s, q)
dijkstraLoop g s q (v : vs) = dijkstraLoop g newSet newQueue vs
  where
    newSet = M.union s (fst nextStep)
    newQueue = merge q (snd nextStep)
    nextStep = dijkstra g s q v

-- one loop for dijk algorithm kanske.
-- g: graph, s : map of visted nodes and distances, q : queue of nodes we are going to visit, v :current node/vertice.
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> Map (Edge a b) b -> SkewHeap (Edge a b) -> a -> (Map (Edge a b) b, SkewHeap (Edge a b))
dijkstra g s q v
  | M.member closest s = (s, delete closest q)
  | otherwise          = (M.insert closest w s, addEdgesToQueue edges w q)
  where
    closest@(Edge _ _ w) = fromMaybe (Edge v v 0) (root q)
    edges = adj v g


-- takes the outgoing edges of x and adds them to the queue, adds x weight to every edge.
addEdgesToQueue :: (Ord a, Ord b, Num b) => [Edge a b] -> b -> SkewHeap (Edge a b) -> SkewHeap (Edge a b)
addEdgesToQueue [] _ queue                          = queue
addEdgesToQueue ((Edge x y w) : edges) weight queue = insert (Edge y x (w+weight)) queue



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
