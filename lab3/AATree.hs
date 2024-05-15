
--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a, O(1)
  get,           -- Ord a => a -> AATree a -> Maybe a, O(log n)
  insert,        -- Ord a => a -> AATree a -> AATree a, O(log n)
  inorder,       -- AATree a -> [a], O(n)
  size,          -- AATree a -> Int, O(n)
  height,        -- AATree a -> Int, O(n)
  checkTree,     -- Ord a => AATree a -> Bool, O(n)       
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node (AATree a) a (AATree a) Level
  deriving (Eq, Show, Read)

type Level = Int

emptyTree :: AATree a
emptyTree = Empty

-- returns the element if it exists in the tree
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get x (Node l y r _)
  |x < y = get x l
  |x > y = get x r
  |otherwise = Just y

-- returns the level of the root of the tree
getlvl :: AATree a -> Level
getlvl Empty = 0
getlvl (Node _ _ _ level) = level

-- if the left child isnt empty, it skews the tree, O(1)
skew :: AATree a -> AATree a
skew tree@(Node Empty _ _ _) = tree
skew tree@(Node (Node a x b ll) y c level)
  | ll == level = Node a x (Node b y c level) level
  | otherwise   = tree

-- splits the tree and creates a node with +1 level, O(1)
split :: AATree a -> AATree a
split tree@(Node a t (Node b r x ll) level)
  | level == getlvl x = Node (Node a t b ll) r x (level + 1)
  | otherwise         = tree

-- if the given tree is empty return a singleton tree
-- if the element already exists in the tree, do nothing
-- if the element is smaller than the root, insert into left child and then skew and split the new tree
-- if the element is larger than the root, insert into right child and then skew and split the new tree
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node Empty x Empty 1
insert x tree@(Node lt y rt level) = case compare x y of
  EQ -> tree
  LT -> fix $ Node (insert x lt) y rt level
  GT -> fix $ Node lt y (insert x rt) level
  where
    fix = split . skew

-- creates a list out of the tree in acending order
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node l x r _) = inorder l ++ [x] ++ inorder r

-- returns the size of the tree aka the number of nodes.
size :: AATree a -> Int
size Empty = 0
size (Node l _ r _) = 1 + size l + size r

-- returns the hight of the tree.
height :: AATree a -> Int
height Empty = 0
height (Node Empty x Empty _) = 0
height (Node lt _ rt _) = 1 + max (height lt) (height rt)

-- Check that an AA tree is ordered and obeys the AA invariants
checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
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
checkLevels :: AATree a -> Bool
checkLevels Empty            = True
checkLevels (Node lt _ rt l) = checkleft lt l && checkright rt l
  where
    checkleft Empty _           = True
    checkleft (Node _ _ _ ll) l = l > ll

    checkright Empty _           = True
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

