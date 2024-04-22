
complexity :: (Double -> Double) -> [Double]
complexity f = map f [100, 101, 200, 10000]

f1 :: Double -> Double
f1 = logBase 2

f2 :: Double -> Double
f2 n = n

f3 :: Double -> Double
f3 n = n * logBase 2 n

f4 :: Double -> Double
f4 n = n^2

f5 :: Double -> Double
f5 n = n^3

f6 :: Double -> Double
f6 n = 2 ** n

fs :: [Double -> Double]
fs = [f1,f2,f3,f4,f5,f6]

ratio :: [Double] -> [Double]
ratio (n:ns) = map (/n) ns

main :: IO ()
main = do
    let ns = map complexity fs
    let timeLimit = 100000
    putStrLn "Ratios:"
    mapM_ (\rs -> putStrLn (unwords (map show rs))) (map ratio ns)
    mapM_ (\rs -> putStrLn (unwords (map show rs))) (ns)
  