{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day8.Circuit (countCircuits, countLast) where

type Node = (Int, Int, Int) -- (X, Y, Z)
newtype Edge = E (Float, Node, Node) -- (Weight, NodeA, NodeB)
data Circuit = C [(Node, Node)] Int -- C (NodeA, NodeB) Size
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

countCircuits :: String -> Int
countCircuits s = getTotal $ joinCircuits $ makeEdges s

countLast :: String -> Int
countLast s = case joinCircuitsUntil $ qsortMax $ edges $ parseNodes s of
              Left ((x,_,_),(x',_,_)) -> x * x'
              Right _                 -> error "No se hizo la cuenta correctamente"

-- AUXS

getTotal :: [Circuit] -> Int
getTotal cs = product $ map size $ take 3 $ qsortMax cs

makeEdges :: String -> [Edge]
makeEdges s = take 1000 $ qsortMin $ edges $ parseNodes s

qsortMax :: Ord a => [a] -> [a]
qsortMax []     = []
qsortMax [x]    = [x]
qsortMax (x:xs) = (qsortMax . larger x) xs ++ [x] ++ (qsortMax . smaller x) xs

qsortMin :: Ord a => [a] -> [a]
qsortMin []     = []
qsortMin [x]    = [x]
qsortMin (x:xs) = (qsortMin . smaller x) xs ++ [x] ++ (qsortMin . larger x) xs

smaller :: Ord a => a -> [a] -> [a]
smaller x = filter (<x)

larger :: Ord a => a -> [a] -> [a]
larger x = filter (>=x)

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z []     = z
recr f z (x:xs) = f x xs (recr f z xs)

-- MAIN LOGIC

parseNodes:: String -> [Node]
parseNodes s = map f $ lines s
    where
        f :: String -> Node
        f s = unlist $ map read $ splitBy ',' s
        splitBy :: Eq a => a -> [a] -> [[a]]
        splitBy x = foldr (\y yss -> if x == y then []:yss else let ys:yss' = yss in (y:ys):yss') [[]]
        unlist :: [a] -> (a, a, a) -- PRECOND: La lista dada debe tener 3 elementos.
        unlist xs = (head xs, xs !! 1, xs !! 2)

edges :: [Node] -> [Edge]
edges = recr (\n ns es -> edgesFrom n ns ++ es) []

edgesFrom :: Node -> [Node] -> [Edge]
edgesFrom nd = map (\nd' -> E (distanceBetween nd nd', nd, nd'))

distanceBetween :: Node -> Node -> Float
distanceBetween (x1,y1,z1) (x2,y2,z2) = sqrt $ fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2

joinCircuits :: [Edge] -> [Circuit]
joinCircuits []               = []
joinCircuits ((E (_,a,b)):es) = (a,b) `add` joinCircuits es
    where
        add :: (Node,Node) -> [Circuit] -> [Circuit]
        add p       []         = [C [p] 2]
        add p@(a,b) tcs@(c:cs)
            | a `appears` c && b `appears` c = tcs
            | a `appears` c = f p b c cs
            | b `appears` c = f p a c cs
            | otherwise = c : p `add` cs
        merge :: Circuit -> Circuit -> Circuit
        merge (C es n) (C es' n') = C (es++es') (n+n')
        f :: (Node,Node) -> Node -> Circuit -> [Circuit] -> [Circuit]
        f p _ c cs = case searchFor a cs of
                     Nothing       -> p `append` c : cs
                     Just (c',cs') -> merge c c' : cs'
        append :: (Node, Node) -> Circuit -> Circuit
        append e (C es n) = C (e:es) (n+1)

searchFor :: Node -> [Circuit] -> Maybe (Circuit, [Circuit])
searchFor _ []     = Nothing
searchFor n (c:cs) = if n `appears` c
                     then Just (c,cs)
                     else case searchFor n cs of
                          Nothing       -> Nothing
                          Just (c',cs') -> Just (c',c:cs')

appears :: Node -> Circuit -> Bool
appears n (C es _) = any (isConnected n) es

isConnected :: Node -> (Node, Node) -> Bool
isConnected n (a,b) = n == a || n == b

size :: Circuit -> Int
size (C _ n) = n

joinCircuitsUntil :: [Edge] -> Either (Node,Node) [Circuit]
joinCircuitsUntil []               = Right []
joinCircuitsUntil ((E (_,a,b)):es) = case joinCircuitsUntil es of
                                     Left p   -> Left p
                                     Right cs -> case (a,b) `add` cs of
                                                 Left p   -> Left p
                                                 Right cs -> Right cs
    where
        add :: (Node,Node) -> [Circuit] -> Either (Node,Node) [Circuit]
        add p       []         = Right [C [p] 2]
        add p@(a,b) tcs@(c:cs)
            | a `appears` c && b `appears` c = Right tcs
            | a `appears` c = f p b c cs
            | b `appears` c = f p a c cs
            | otherwise = case p `add` cs of
                          Left p'  -> Left p'
                          Right cs -> Right $ c : cs
        merge :: Circuit -> Circuit -> (Bool, Circuit)
        merge (C es n) (C es' n') = (n+n' >= 1000, C (es++es') (n+n'))
        f :: (Node,Node) -> Node -> Circuit -> [Circuit] -> Either (Node,Node) [Circuit]
        f p x c cs = case searchFor x cs of
                     Nothing       -> let (b', cr) = p `append` c
                                       in if b' then Left p else Right $ cr : cs
                     Just (c',cs') -> let (b', cr) = merge c c'
                                     in if b' then Left p else Right $ cr : cs'
        append :: (Node, Node) -> Circuit -> (Bool, Circuit)
        append e (C es n) = (n+1 >= 1000, C (e:es) (n+1))