import Data.List
reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]

prop_reverseListTest :: [Int] -> Bool
prop_reverseListTest xs = reverseList (reverseList xs) == xs

interleave2 :: String -> String -> String
interleave2 [] [] = []
interleave2 [] ys = ys
interleave2 xs [] = xs
interleave2 (x:xs) (y:ys) = x : y : interleave2 xs ys

interleave3 :: String -> String -> String -> String
interleave3 [] [] [] = []
interleave3 xs [] [] = xs
interleave3 [] ys [] = ys 
interleave3 [] [] zs = zs
interleave3 xs ys [] = interleave2 xs ys
interleave3 xs [] zs = interleave2 xs zs
interleave3 [] ys zs = interleave2 ys zs
interleave3 (x:xs) (y:ys) (z:zs) = x : y : z : interleave3 xs ys zs

main :: IO ()
main = do 
    let s1 = "Didrik"
        s2 = "Gustav__"
        s3 = "!!!!!!!!!"
    putStrLn $ interleave3 s1 s2 s3

interleave5 :: String -> String -> String -> String
interleave5 xs ys zs = foldr (\(a,b,c) acc -> a:b:c:acc) [] (zip3 xs ys zs)

main5 :: IO ()
main5 = do 
    let s1 = "Didrik"
        s2 = "Gustav__"
        s3 = "!!!!!!!!!"
    putStrLn $ interleave5 s1 s2 s3



interleave6 :: [String] -> String 
interleave6 [] = []
interleave6 strings = concat (transpose strings)

main6 :: IO ()
main6 = do 
    let strings = ["Didrik", "Gus", "!!!!!!!!!!!!!", "PENIS", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
    putStrLn $ interleave6 strings