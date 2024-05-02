
type Stack a = [a]

push :: a -> Stack a -> Stack a
push x xs = x:xs

pop :: Stack a -> Stack a
pop (x:xs) = xs

top :: Stack a -> a
top (x:xs) = x

empty :: Stack a -> Bool
empty [] = True
empty (x:xs) = False

-- Precondition: String is written in RPS
