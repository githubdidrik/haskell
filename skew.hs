module Skew
  ( SkewHeap(..)
  , empty
  , singleton
  , merge
  , root
  , insert
  , delete
  , toString
  ) where

data SkewHeap a = Empty | Node (SkewHeap a) a (SkewHeap a) deriving (Show)

empty :: SkewHeap a
empty = Empty

singleton :: Ord a => a -> SkewHeap a
singleton x = Node Empty x Empty

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty h  = h
merge h Empty  = h
merge h1@(Node a1 x1 b1) h2@(Node a2 x2 b2)
    |x1 <= x2   = Node (merge h2 b1) x1 a1
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

toString :: (Show a, Ord a) => SkewHeap a -> String
toString Empty                 = ""
toString (Node Empty a Empty)  = show a ++ ", "
toString (Node l a Empty)  = show a ++ ", " ++ toString l
toString (Node Empty a r)  = show a ++ ", " ++ toString r
toString (Node l a r)
    | root l <= root r   = show a ++ ", " ++ toString r ++ toString l
    | otherwise          = show a ++ ", " ++ toString l ++ toString r

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
