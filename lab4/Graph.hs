
module Graph
  ( -- * Edge
    Edge(..)                    -- type
  , src, dst, label         -- querying an Edge

    -- * Graph
  , Graph(..)                   -- type
  , empty                   -- create an empty map
  , addVertex, addVertices  -- adding vertices (nodes)
  , addEdge, addBiEdge      -- adding edges (one way or both ways)
  , adj                     -- get adjacent nodes for a given node
  , vertices, edges         -- querying a Graph
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- An edge with a source and destination node (of type a), 
-- together with a label of type b
data Edge a b = Edge
  { src   :: a  -- ^ Source vertex
  , dst   :: a  -- ^ Destination vertex
  , label :: b  -- ^ The label
  } deriving (Show)

-- A graph with nodes of type a and labels of type b.
data Graph a b = Graph (Map a [Edge a b]) deriving Show
  -- TODO: implement a graph with adjacency lists, hint: use a Map.

instance (Ord a, Ord b) => Ord (Edge a b) where
  (Edge _ _ d1) <= (Edge _ _ d2) = d1 <= d2

instance (Eq a, Eq b) => Eq (Edge a b) where
  (Edge x1 z1 d1) == (Edge x2 z2 d2) = x1 == x2 && z1 == z2 && d1 == d2

-- | Create an empty graph
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v (Graph g)
  | M.member v g = Graph g
  | otherwise    = Graph (M.insert v [] g)

-- | Add a list of vertices to a graph
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices vs g = foldr addVertex g vs

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l (Graph g)
  | M.member v g && M.member w g = Graph (M.insertWith (++) v [Edge v w l] g)
  | otherwise                    = Graph g

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l g = addEdge v w l (addEdge w v l g)

-- | Get all adjacent vertices (nodes) for a given node. GET ALL EDGES!!!
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v (Graph g) = fromMaybe [] $ M.lookup v g

-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices (Graph g) = M.keys g 

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges (Graph g) = concat (M.elems g)


g1 = addBiEdge 1 10 "ettTillTio" (addVertices [1..10] empty)