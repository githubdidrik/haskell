module Skew
  ( SkewHeap(..)
  , singleton -- creates an empty skewheap. O(1)
  , merge -- merges two skewheaps into one. O(log n)
  , root -- returns the root of the tree, which is the element in the front of the prio queue. O(1)
  , insert -- inserts an element in a heap by merging, therefore O(log n)
  , delete -- searches through the heap for the element, if found it merges the children. the search takes worst case O(n). 
  , toString -- goes through every element in the heap, this takes O(n). for every element it merges its children which takes O(log n). 
  , isEmpty
  ) where    -- worst case toString is O(n log n). 

data SkewHeap a = Empty | Node (SkewHeap a) a (SkewHeap a) deriving (Show)

singleton :: Ord a => a -> SkewHeap a
singleton x = Node Empty x Empty

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty h  = h
merge h Empty  = h
merge h1@(Node a1 x1 b1) h2@(Node a2 x2 b2)
    |x1 < x2    = Node (merge h2 b1) x1 a1
    |otherwise  = Node (merge h1 b2) x2 a2

root :: SkewHeap a -> Maybe a
root Empty        = Nothing
root (Node _ a _) = Just a

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x = merge (singleton x)

delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete _ Empty = Empty
delete x (Node l y r)
    |x == y    = merge l r
    |otherwise = Node (delete x l) y (delete x r)

isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

{- toString :: (Show a, Ord a) => SkewHeap a -> String
toString Empty = ""
toString (Node l x r) = show x ++ ", " ++ toString (merge l r) -}

toString :: (Show a, Ord a) => SkewHeap a -> String
toString heap = go heap ""
    where
        go Empty acc = acc
        go (Node l x r) acc = show x ++ ", " ++ go (merge l r) acc


invariant :: Ord a => SkewHeap a -> Bool
invariant Empty                = True
invariant (Node Empty _ Empty) = True
invariant (Node l x Empty)     = case root l of
    Just lx            -> lx <= x && invariant l
    _                  -> False
invariant (Node l x r)         = case (root l, root r) of  
    (Just lx, Just rx) -> lx <= x && rx <= x && invariant l && invariant r
    _                  -> False
        
prop_invariant :: Ord a => [a] -> Bool
prop_invariant xs = invariant (fromList xs)

fromList :: Ord a => [a] -> SkewHeap a
fromList []     = Empty
fromList [x]    = singleton x
fromList (x:xs) = insert x $ fromList xs 
