module Day8.Circuit where

import Data.List (sort)

type Node = (Int, Int, Int) -- (X, Y, Z)
newtype Edge = E (Float, Int, Int) -- (Weight, NodeA, NodeB)
data Circuit = C [Edge] Int -- C Edges Size
    
instance Show Edge where
    show :: Edge -> String
    show (E e) = show e

instance Ord Edge where
    (>=) :: Edge -> Edge -> Bool
    (>=) (E (x,_,_)) (E (y,_,_)) = x >= y
    (<=) (E (x,_,_)) (E (y,_,_)) = x <= y

instance Eq Edge where
    (==) :: Edge -> Edge -> Bool
    (==) (E (x,_,_)) (E (y,_,_)) = x == y

test :: IO ()
test = do ss <- readFile "./app/Day8/input.txt"
          print $ take 10 $ sort $ edges $ parseNodes ss

test2 :: IO ()
test2 = do ss <- readFile "./app/Day8/input.txt"
           print $ length $ edges $ parseNodes ss

parseNodes:: String -> [Node]
parseNodes s = map f $ lines s
    where
        f :: String -> Node
        f s = unlist $ map read $ splitBy ',' s
        splitBy :: Eq a => a -> [a] -> [[a]]
        splitBy x = foldr (\y yss -> if x == y then []:yss else let ys:yss' = yss in (y:ys):yss') [[]]
        unlist :: [a] -> (a, a, a)
        -- PRECOND: La lista dada debe tener 3 elementos.
        unlist xs = (head xs, xs !! 1, xs !! 2)

edges :: [Node] -> [Edge]
edges ns = f [0..] ns
    where
        f :: [Int] -> [Node] -> [Edge]
        f _        []     = []
        f _        (_:[]) = []
        f (id:ids) (n:ns) = (edgesFrom n ns id ids) ++ f ids ns

edgesFrom :: Node -> [Node] -> Int -> [Int] -> [Edge]
edgesFrom nd []        n ns = []
edgesFrom nd (nd':nds) n ns = E (distanceBetween nd nd', n, head ns) : edgesFrom nd nds n (tail ns)

distanceBetween :: Node -> Node -> Float
distanceBetween (x1,y1,z1) (x2,y2,z2) = sqrt $ fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2

uniteInCircuits :: [Edge] -> [Circuit]
uniteInCircuits []     = []
uniteInCircuits (e:es) = f e es : uniteInCircuits es
    where
        f :: Edge -> [Edge] -> Circuit
        f (x,a,b) = filter (\e -> union (p1 (x,a,b)) (p2 ??) e)
        p1 :: Edge -> Edge -> Bool
        p1 (x,a,b) (x',a',b') = a == a' || a == b' || b == a' || b == b'
        p2 :: Edge -> Edge -> Bool
        p2 (x,a,b) (x',a',b') = (a == a' <= b /= b') || (a == b' <= b /= a')

join :: Circuit -> Circuit -> Circuit
join (C es n) (C es' n') = C (es++es') (n+n')

add :: Edge -> Circuit -> Circuit
add e (C es n) = C (e:es) (n+1)

union :: (a -> Bool) -> (a -> Bool) -> a -> Bool
union p1 p2 = \x -> p1 x || p2 x