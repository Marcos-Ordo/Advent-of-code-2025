module Day8.Circuit where

type Node = (Int, Int, Int) -- (X, Y, Z)
newtype Edge = E (Float, Int, Int) -- (Weight, NodeA, NodeB)
data Circuit = C [(Int, Int)] Int -- C (EdgeA, EdgeB) Size
    deriving (Show)

instance Show Edge where
    show (E e) = show e

instance Ord Edge where
    (>=) (E (x,_,_)) (E (y,_,_)) = x >= y
    (<=) (E (x,_,_)) (E (y,_,_)) = x <= y

instance Eq Edge where
    (==) (E (x,_,_)) (E (y,_,_)) = x == y

instance Ord Circuit where
    (>=) (C _ n) (C _ n') = n >= n'
    (<=) (C _ n) (C _ n') = n <= n'

instance Eq Circuit where
    (==) (C _ n) (C _ n') = n == n'

test :: IO ()
test = do s <- readFile "./app/Day8/input.txt"
          print $ makeEdges s

test2 :: IO ()
test2 = do s <- readFile "./app/Day8/input.txt"
           print $ joinCircuits $ makeEdges s

test3 :: IO ()
test3 = do s <- readFile "./app/Day8/input.txt"
           print $ getTotal $ joinCircuits $ makeEdges s

test4 :: IO ()
test4 = do s <- readFile "./app/Day8/input.txt"
           print $ getTotal $ joinCircuits $ qsortMin $ edges $ parseNodes s

countCircuits :: String -> Int
countCircuits s = getTotal $ joinCircuits $ makeEdges s

-- AUXS

countCircuitsTest :: [Edge] -> [Int]
countCircuitsTest = map size . joinCircuits

getTotal :: [Circuit] -> Int
getTotal cs = product $ map size $ take 3 $ qsortMax cs

makeEdges :: String -> [Edge]
makeEdges s = take 1000 $ qsortMin $ edges $ parseNodes s

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
        f _        [_]    = []
        f (id:ids) (n:ns) = edgesFrom n ns id ids ++ f ids ns

edgesFrom :: Node -> [Node] -> Int -> [Int] -> [Edge]
edgesFrom nd []        n ns = []
edgesFrom nd (nd':nds) n ns = E (distanceBetween nd nd', n, head ns) : edgesFrom nd nds n (tail ns)

distanceBetween :: Node -> Node -> Float
distanceBetween (x1,y1,z1) (x2,y2,z2) = sqrt $ fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2

joinCircuits :: [Edge] -> [Circuit]
joinCircuits []               = []
joinCircuits ((E (_,a,b)):es) = (a,b) `add` joinCircuits es
    where
        add :: (Int,Int) -> [Circuit] -> [Circuit]
        add e       []         = [C [e] 2]
        add e@(a,b) tcs@(c:cs)
            | a `appears` c && b `appears` c = tcs
            | a `appears` c = case searchFor b cs of
                              Nothing       -> e `append` c : cs
                              Just (c',cs') -> merge c c' : cs'
            | b `appears` c = case searchFor a cs of
                              Nothing       -> e `append` c : cs
                              Just (c',cs') -> merge c c' : cs'
            | otherwise = c : e `add` cs

searchFor :: Int -> [Circuit] -> Maybe (Circuit, [Circuit])
searchFor n []     = Nothing
searchFor n (c:cs) = if n `appears` c
                     then Just (c,cs)
                     else case searchFor n cs of
                          Nothing       -> Nothing
                          Just (c',cs') -> Just (c',c:cs')

merge :: Circuit -> Circuit -> Circuit
merge (C es n) (C es' n') = C (es++es') (n+n')

append :: (Int, Int) -> Circuit -> Circuit
append e (C es n) = C (e:es) (n+1)

appears :: Int -> Circuit -> Bool
appears n (C es _) = any (isConnected n) es

isConnected :: Int -> (Int, Int) -> Bool
isConnected n (a,b) = n == a || n == b

takeBy :: (a -> Bool) -> [a] -> ([a],[a])
takeBy f = foldr (\x (ts, ds) -> if f x then (x:ts,ds) else (ts,x:ds)) ([],[])

size :: Circuit -> Int
size (C _ n) = n

qsortMax :: Ord a => [a] -> [a]
qsortMax []     = []
qsortMax [x]    = [x]
qsortMax (x:xs) = qsortMax larger ++ [x] ++ qsortMax smaller
    where
        smaller = filter (<x) xs
        larger = filter (>=x) xs

qsortMin :: Ord a => [a] -> [a]
qsortMin []     = []
qsortMin [x]    = [x]
qsortMin (x:xs) = qsortMin smaller ++ [x] ++ qsortMin larger
    where
        smaller = filter (<x) xs
        larger = filter (>=x) xs