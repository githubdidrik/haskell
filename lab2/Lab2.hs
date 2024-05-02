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
  deriving (Eq)
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

type OrderBook = (SkewHeap Bid, SkewHeap Bid)

emptyOrderBook :: OrderBook
emptyOrderBook = (Empty, Empty)

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
    (Buy _ p1)  <= (Sell _ p2) = p1 >= p2
    (Sell _ p1) <= (Buy _ p2) = p1 <= p2

-- goes through all the bids and then prints the final orderbook
trade :: OrderBook -> [Bid] -> IO()
trade orderBook [] = do
    putStrLn "Orderbook:"
    putStrLn $ "Sellers: " ++ toString (snd orderBook)
    putStrLn $ "Buyers: "  ++ toString (fst orderBook)
trade orderBook (bid : rest) = do
    newOrderBook@(buyHeap, sellHeap) <- execute bid orderBook
    trade newOrderBook rest

-- matches type of bid to the right helper function
execute :: Bid -> OrderBook -> IO OrderBook
execute bid book = case bid of
    (Buy p pr)          -> doBuyBid bid book
    (Sell p pr)         -> doSellBid bid book
    (NewBuy p old new)  -> doNewBuyBid bid book
    (NewSell p old new) -> doNewSellBid bid book

-- checks if the buyer can purchase a stock, if so it prints a message and returns the orderbook without the corresponding sell bid. 
doBuyBid :: Bid -> OrderBook -> IO OrderBook
doBuyBid buybid book = 
    if buybid < lowestsellbid
    then do
        tradeMessage buybid lowestsellbid
        return (deleteBid lowestsellbid book)
    else return (addBid buybid book)
        where
            lowestsellbid = case root (snd book) of 
                Just bid -> bid
                _        -> buybid

-- checks if the seller can sell a stock, if so it prints a message and returns the orderbook without the corresponding buy id
doSellBid :: Bid -> OrderBook -> IO OrderBook
doSellBid sellbid book =
    if sellbid < highestbuybid
    then do
        tradeMessage highestbuybid sellbid
        return (deleteBid highestbuybid book)
    else return (addBid sellbid book)
        where
            highestbuybid = case root (fst book) of 
                Just bid -> bid
                _        -> sellbid

-- removes the old bid and then runs doBuyBid for the new bid.
doNewBuyBid :: Bid -> OrderBook -> IO OrderBook
doNewBuyBid (NewBuy person old new) book = doBuyBid (Buy person new) (deleteBid (Buy person old) book)

-- removes the old bid and then runs doSellBid for the new bid.
doNewSellBid :: Bid -> OrderBook -> IO OrderBook
doNewSellBid (NewSell person old new) book = doSellBid (Sell person new) (deleteBid (Sell person old) book)


tradeMessage :: Bid -> Bid -> IO()
tradeMessage (Buy buyer p) (Sell seller _) = do
    putStrLn $ buyer ++ " buys from " ++ seller ++ " for " ++ show p ++ " kr"


