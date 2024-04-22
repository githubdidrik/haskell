{-# LANGUAGE MonoLocalBinds #-}
data SkewHeap a = Empty | Node (SkewHeap a) a (SkewHeap a) deriving (Show)

data Bid       = Bid String Int deriving (Show, Eq)
type BuyHeap   = SkewHeap Bid
type SellHeap  = SkewHeap Bid
type OrderBook = (BuyHeap, SellHeap)

empty :: SkewHeap a
empty = Empty

singleton :: Ord a => a -> SkewHeap a
singleton x = Node Empty x Empty

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a 
merge Empty h  = h
merge h Empty  = h
merge h1@(Node a1 x1 b1) h2@(Node a2 x2 b2)
    |x1 >= x2   = Node (merge h2 b1) x1 a1
    |otherwise  = Node (merge h1 b2) x2 a2

mergeB :: Ord Bid => BuyHeap -> BuyHeap -> BuyHeap
mergeB Empty h  = h
mergeB h Empty  = h
mergeB h1@(Node a1 x1 b1) h2@(Node a2 x2 b2)
    |x1 >= x2   = Node (mergeB h2 b1) x1 a1
    |otherwise  = Node (mergeB h1 b2) x2 a2

mergeS :: Ord Bid => SellHeap -> SellHeap -> SellHeap
mergeS Empty h  = h
mergeS h Empty  = h
mergeS h1@(Node a1 x1 b1) h2@(Node a2 x2 b2)
    |x1 <= x2   = Node (mergeS h2 b1) x1 a1
    |otherwise  = Node (mergeS h1 b2) x2 a2

delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete _ Empty = Empty
delete x (Node l y r)
    |x == y    = merge l r
    |x > y     = Node (delete x l) y r
    |otherwise = Node l y (delete x r)

deleteB :: Ord Bid => Bid -> BuyHeap -> BuyHeap
deleteB _ Empty = Empty
deleteB x (Node l y r)
    |x == y    = mergeB l r
    |x > y     = Node (deleteB x l) y r
    |otherwise = Node l y (deleteB x r)

deleteS :: Ord Bid => Bid -> SellHeap -> SellHeap
deleteS _ Empty = Empty
deleteS x (Node l y r)
    |x == y    = mergeS l r
    |x < y     = Node (deleteS x l) y r
    |otherwise = Node l y (deleteS x r)

toString :: SkewHeap Bid -> String
toString Empty                                = ""
toString (Node Empty (Bid name price) Empty)  = name ++ " " ++ show price
toString (Node l (Bid name price) r)          = name ++ " " ++ show price ++ ", " ++ toString l ++ toString r

addBuyBid :: Bid -> OrderBook -> OrderBook
addBuyBid bid (buyHeap, sellHeap) = (mergeB (singleton bid) buyHeap, sellHeap)


addSellBid :: Bid -> OrderBook -> OrderBook
addSellBid bid (buyHeap, sellHeap) = (buyHeap, mergeS (singleton bid) sellHeap)

instance Ord Bid where
    --(Bid n1 price1) == (Bid n2 price2) = n1 == n2
    (Bid _ price1) <= (Bid _ price2) = price1 <= price2 
    (Bid _ price1) >= (Bid _ price2) = price1 >= price2
    (Bid _ price1) < (Bid _ price2)  = price1 < price2
    (Bid _ price1) > (Bid _ price2)  = price1 > price2


--OrderBook -> [Bid] -> IO




--acending/sell
heap :: SkewHeap Int 
heap = Node (Node Empty 9 (singleton 11)) 8 (singleton 10)
heap1 :: SkewHeap Int 
heap1 = Node (Node Empty 18 (singleton 22)) 16 (singleton 20)

--decending/buy
he1 :: SkewHeap Int
he1 = Node (Node Empty 9 (singleton 8)) 11 (singleton 10)
he2 :: SkewHeap Int 
he2 = Node (Node Empty 18 (singleton 16)) 22 (singleton 20)


buyBid1, buyBid2, buyBid3 :: Bid
buyBid1 = Bid "Didrik" 70
buyBid2 = Bid "Gustav" 65
buyBid3 = Bid "Samuel" 200
sellBid1, sellBid2, sellBid3 :: Bid
sellBid1 = Bid "Josef" 75
sellBid2 = Bid "Victor" 80
sellBid3 = Bid "Oliwer" 10


test :: IO()
test = do
    let orderBook  = (empty, empty) :: OrderBook
        orderBook1 = addBuyBid buyBid1 orderBook
        orderBook2 = addSellBid sellBid1 orderBook1
    print orderBook2

test1 :: IO()
test1 = do
    let buyHeap1  = singleton buyBid1
        buyHeap2  = singleton buyBid2
        sellHeap1 = singleton sellBid1
        sellHeap2 = singleton sellBid2
        mergeBuy  = mergeB buyHeap1 buyHeap2
        mergeSell = mergeS sellHeap1 sellHeap2
        mergeB2   = mergeB mergeBuy (singleton buyBid3)
        mergeS2   = mergeS mergeSell (singleton sellBid3)
    print mergeBuy
    print mergeSell
    print mergeB2
    print $ toString mergeB2
    print $ toString mergeS2





