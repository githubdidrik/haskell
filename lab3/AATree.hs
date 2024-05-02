
--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node (AATree a) a (AATree a) Level
  deriving (Eq, Show, Read)

type Level = Int
emptyTree :: AATree a
emptyTree = Empty

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get x (Node l y r _)
  |x < y = get x l
  |x > y = get x r
  |otherwise = Just y

-- You may find it helpful to define
--   split :: AATree a -> AATree a
--   skew  :: AATree a -> AATree a
-- and call these from insert.

getlvl :: AATree a -> Level
getlvl Empty = 0
getlvl (Node _ _ _ level) = level

skew :: AATree a -> AATree a
skew Empty = Empty
skew tree@(Node Empty _ _ _) = tree
skew tree@(Node (Node a x b ll) y c level)
  | ll == level = Node a x (Node b y c level) level
  | otherwise   = tree

split :: AATree a -> AATree a
split tree@(Node a t (Node b r x ll) level)
  | level == getlvl x = Node (Node a t b ll) r x (level + 1)
  | otherwise         = tree

insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node Empty x Empty 1
insert x tree@(Node lt y rt level) = case get x tree of
  Just x -> tree
  _      ->
    if x < y
      then skew $ Node (insert x lt) y rt (1 + max (getlvl lt) (getlvl rt))
      else skew $ Node lt y (insert x rt) (1 + max (getlvl lt) (getlvl rt))

tree1 = insert 5 (insert 3 (insert 7 emptyTree))

tree2 = foldr insert emptyTree [1..100]

tree3 = Node (Node (Node Empty 3 Empty 1) 5 Empty 2) 7 (Node Empty 9 Empty 1) 3

tree4 = foldr insert emptyTree ["The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"]

inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node l x r _) = inorder l ++ [x] ++ inorder r

size :: AATree a -> Int
size Empty = 0
size (Node l _ r _) = 1 + size l + size r

height :: AATree a -> Int
height Empty = 0
height (Node lt _ rt _) = 1 + max (height lt) (height rt)

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels1 (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered


isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)


-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant

checkLevels1 :: AATree a -> Bool
checkLevels1 Empty = True
checkLevels1 (Node lt _ rt l) = checkleft lt l && checkright rt l && checkLevels1 lt && checkLevels1 rt
  where
    checkleft Empty _ = True
    checkleft (Node _ _ _ ll) l = l > ll

    checkright Empty _ = True
    checkright (Node _ _ rt _) l = l > getlvl rt

isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _     = False 

leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node lt _ _ _) = lt

rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ _ rt _) = rt

--------------------------------------------------------------------------------

