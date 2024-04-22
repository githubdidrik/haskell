

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
root Empty = Nothing 
root (Node _ a _) = Just a

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x = merge (singleton x)

delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete _ Empty = Empty
delete x (Node l y r)
    |x == y    = merge l r
    |otherwise = Node (delete x l) y (delete x r)

toString :: SkewHeap Bid -> String
toString Empty                 = ""
toString (Node Empty a Empty)  = show a ++ ", "
toString (Node l a Empty)  = show a ++ ", " ++ toString l
toString (Node Empty a r)  = show a ++ ", " ++ toString r
toString (Node l@(Node _ x1 _) a r@(Node _ x2 _))
    | x1 <= x2   = show a ++ ", " ++ toString r ++ toString l
    | otherwise  = show a ++ ", " ++ toString l ++ toString r

instance Show Bid where
    show (Buy person price) = person ++ " " ++ show price
    show (Sell person price) = person ++ " " ++ show price

data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid


type Person = String
type Price = Integer

instance Ord Bid where
    (Buy _ p1) <= (Buy _ p2) = p1 <= p2
    (Sell _ p1) <= (Sell _ p2) = p1 >= p2

instance Eq Bid where
    (Buy _ p1) == (Buy _ p2) = p1 == p2
    (Sell _ p1) == (Sell _ p2) = p1 == p2

type OrderBook = (SkewHeap Bid, SkewHeap Bid)

addBid :: Bid -> OrderBook -> OrderBook
addBid b@(Buy _ _)  (buyheap, sellheap) = (insert b buyheap, sellheap)
addBid s@(Sell _ _) (buyheap, sellheap) = (buyheap, insert s sellheap)

deleteBid :: Bid -> OrderBook -> OrderBook
deleteBid b@(Buy _ _)  (buyheap, sellheap) = (delete b buyheap, sellheap)
deleteBid s@(Sell _ _) (buyheap, sellheap) = (buyheap, delete s sellheap)



trade :: OrderBook -> [Bid] -> IO()
trade orderBook [] = do
    let (buyHeap, sellHeap) = orderBook
    putStrLn "Final status of orderbook:"
    putStrLn $ "Buyers: " ++ toString buyHeap
    putStrLn $ "Sellers: " ++ toString sellHeap
trade orderBook (bid : rest) = do
    newOrderBook@(buyHeap, sellHeap) <- execute bid orderBook
    putStrLn "Currrent status of orderbook:"
    putStrLn $ "Buyers: " ++ toString buyHeap
    putStrLn $ "Sellers: " ++ toString sellHeap
    putStrLn ""
    trade (deleteBid bid newOrderBook) rest
          

execute :: Bid -> OrderBook -> IO OrderBook
execute bid book@(buybids, sellbids) = case bid of
    (Buy p price) -> case root sellbids of
        Just (Sell p askingPrice) -> if price >= askingPrice
            then do
                doTrade bid (Sell p askingPrice)
                let newSellbids = delete (Sell p askingPrice) sellbids
                return (buybids, newSellbids)
            else return (insert bid buybids, sellbids)
        Nothing -> return (insert bid buybids, sellbids)

    (Sell _ price) -> case root buybids of
        Just (Buy p biddingPrice) -> if price <= biddingPrice
            then do
                doTrade (Buy p biddingPrice) bid
                let newBuybids = delete (Buy p biddingPrice) buybids
                return (newBuybids, sellbids)
            else return (buybids, insert bid sellbids)
        Nothing -> return (insert bid buybids, sellbids)

    (NewBuy person oldPrice newPrice) ->
        let newBuybids = delete (Buy person oldPrice) buybids
            newBuybid = Buy person newPrice
        in return (insert newBuybid newBuybids, sellbids)

    (NewSell person oldPrice newPrice) -> 
        let newSellbids = delete (Sell person oldPrice) sellbids
            newSellbid = Sell person newPrice
        in return (buybids, insert newSellbid newSellbids)


        

doTrade :: Bid -> Bid -> IO()
doTrade b@(Buy buyer p) s@(Sell seller _) = do
    putStrLn $ buyer ++ " buys a share from " ++ seller ++ " for " ++ show p


main :: IO ()
main = do 
    trade book bids
    putStrLn "Trading completed."


h1 :: SkewHeap Bid -- buybid/decending
h1 = Node (Node Empty buyBid1 (singleton buyBid2)) buyBid3 (singleton buyBid4)
h2 :: SkewHeap Bid -- sellbid/ascending
h2 = Node (Node Empty sellBid1 (singleton sellBid2)) sellBid3 (singleton sellBid4)

book :: OrderBook
book = (h1, h2)

bids :: [Bid]
bids = [Buy "Aguy" 100, Sell "Someguy" 200, NewBuy "Didrik" 70 75]

buyBid1, buyBid2, buyBid3, buyBid4 :: Bid
buyBid1 = Buy "Didrik" 70
buyBid2 = Buy "Gustav" 65
buyBid3 = Buy "Samuel" 200
buyBid4 = Buy "Melker" 100
sellBid1, sellBid2, sellBid3, sellBid4 :: Bid
sellBid1 = Sell "Josef" 80
sellBid2 = Sell "Victor" 100
sellBid3 = Sell "Oliwer" 10
sellBid4 = Sell "Nils" 50

