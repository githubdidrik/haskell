module Lab2 where
import Skew
import Control.Applicative
import System.Environment
import System.IO

import Test.QuickCheck

-- | Bids.

data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid

type Person = String
type Price = Integer

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right (Buy name price)
      ("S",  Just [price])              -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = tradeMain =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.

tradeMain :: [Bid] -> IO ()
tradeMain = trade emptyOrderBook

emptyOrderBook :: OrderBook
emptyOrderBook = (Empty, Empty)

type OrderBook = (SkewHeap Bid, SkewHeap Bid)

addBid :: Bid -> OrderBook -> OrderBook
addBid b (buyheap, sellheap) = case b of
    (Buy _ _)  -> (insert b buyheap, sellheap)
    (Sell _ _) -> (buyheap, insert b sellheap)

deleteBid :: Bid -> OrderBook -> OrderBook
deleteBid b (buyheap, sellheap) = case b of
    (Buy _ _)  -> (delete b buyheap, sellheap)
    (Sell _ _) -> (buyheap, delete b sellheap)

instance Show Bid where
    show (Buy person price)  = person ++ " " ++ show price
    show (Sell person price) = person ++ " " ++ show price

instance Ord Bid where
    (Buy _ p1)  <= (Buy _ p2) = p1 <= p2
    (Sell _ p1) <= (Sell _ p2) = p1 >= p2

instance Eq Bid where
    (Buy _ p1)       == (Buy _ p2)       = p1 == p2
    (Sell _ p1)      == (Sell _ p2)      = p1 == p2
    (NewBuy _ p1 _)  == (NewBuy _ p2 _)  = p1 == p2
    (NewSell _ p1 _) == (NewSell _ p2 _) = p1 == p2
    _                == _                = False


trade :: OrderBook -> [Bid] -> IO()
trade orderBook [] = do
    putStrLn "\nOrderbok:"
    putStrLn $ "Säljare: " ++ toString (snd orderBook)
    putStrLn $ "Köpare: " ++ toString (fst orderBook)
trade orderBook (bid : rest) = do
    newOrderBook@(buyHeap, sellHeap) <- execute bid orderBook
    trade newOrderBook rest
 

execute :: Bid -> OrderBook -> IO OrderBook
execute bid book@(Empty, Empty)    = return (addBid bid book)

execute bid book@(Empty, sellbids) = case bid of
    (Buy p price)  -> case root sellbids of 
        Just (Sell p askingPrice) -> if price >= askingPrice
            then do
                doTrade bid (Sell p askingPrice)
                let newOrderBook = deleteBid bid (deleteBid (Sell p askingPrice) book)
                return newOrderBook
            else return $ addBid bid book
        _ -> return book -- borde inte ské

    (Sell p price) -> return (addBid bid book)

    (NewSell person oldPrice newPrice) ->         
        let newSellbid = Sell person newPrice
            oldSellbid = Sell person oldPrice
            newOrderBook = addBid newSellbid (deleteBid oldSellbid book)
        in return newOrderBook

        
execute bid book@(buybids, Empty) = case bid of
    (Sell p price)  -> case root buybids of
        Just (Buy p biddingPrice) -> if price <= biddingPrice
            then do
                doTrade (Buy p biddingPrice) bid
                let newOrderBook = deleteBid bid (deleteBid (Buy p biddingPrice) book)
                return newOrderBook
            else return $ addBid bid book
        _ -> return book -- borde inte ské

    (Buy p price) -> return (addBid bid book)

    (NewBuy person oldPrice newPrice) ->         
        let newBuybid = Buy person newPrice
            oldBuybid = Buy person oldPrice
            newOrderBook = addBid newBuybid (deleteBid oldBuybid book)
        in return newOrderBook


execute bid book@(buybids, sellbids) = case bid of
    (Buy p price) -> case root sellbids of
        Just (Sell p askingPrice) -> if price >= askingPrice
            then do
                doTrade bid (Sell p askingPrice)
                let newOrderBook = deleteBid bid (deleteBid (Sell p askingPrice) book)
                return newOrderBook
            else return $ addBid bid book
        _ -> return book -- borde inte ske

    (Sell _ price) -> case root buybids of
        Just (Buy p biddingPrice) -> if price <= biddingPrice
            then do
                doTrade (Buy p biddingPrice) bid
                let newOrderBook = deleteBid bid (deleteBid (Buy p biddingPrice) book)
                return newOrderBook
            else return $ addBid bid book
        _ -> return book -- borde inte ské

    (NewBuy person oldPrice newPrice) ->
        let newBuybid = Buy person newPrice
            oldBuybid = Buy person oldPrice
            newOrderBook = addBid newBuybid (deleteBid oldBuybid book)
        in return newOrderBook

    (NewSell person oldPrice newPrice) ->
        let newSellbid = Sell person newPrice
            oldSellbid = Sell person oldPrice
            newOrderBook = addBid newSellbid (deleteBid oldSellbid book)
        in return newOrderBook


doTrade :: Bid -> Bid -> IO()
doTrade b@(Buy buyer p) s@(Sell seller _) = do
    putStrLn $ buyer ++ " buys a share from " ++ seller ++ " for " ++ show p





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

