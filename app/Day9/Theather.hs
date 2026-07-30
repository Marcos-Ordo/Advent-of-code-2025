{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TupleSections #-}

module Day9.Theather where 

type Pos = (Int,Int) -- (Eje X, Eje Y)
data Intervalo = I Int (Int -> Bool) -- (Eje Y / nivel) (Intervalo X)

getBiggestArea :: String -> Int
getBiggestArea = biggestArea . parsePos

getBiggestInsider :: String -> (Pos,Pos)
getBiggestInsider s = let ps  = parsePos s
                          f   = intervalosCruzados ps
                          ps' = pairs ps
                       in biggestInsider f ps'

-- TESTS

liveTest :: IO ()
liveTest = do s <- readFile "./app/Day9/input.txt"
              print $ biggestInsider (f s) (ps' s)
    where
        ps :: String -> [Pos]
        ps s = parsePos s
        f :: String -> Int -> Pos -> Int
        f = intervalosCruzados . ps
        ps' :: String -> [(Pos,Pos)]
        ps' = pairs . ps


-- EXAMPLES

textExample :: String
textExample = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"

example :: [Pos]
example = [(7,1),(11,1),(11,7),(9,7),(9,5),(2,5),(2,3),(7,3)]

example2 :: [Pos]
example2 = [(1,1),(5,1),(5,3),(8,3),(3,6),(8,6),(1,8),(3,8)]

example3 :: [Pos]
example3 = [(7,1),(11,1),(2,3),(7,3),(2,5),(9,5),(9,7),(11,7)]

example4 :: [Pos]
example4 = [(0,0),(8,0),(8,5),(7,5),(7,7),(6,7),(6,1),(5,1),(5,2),(4,2),(4,4),(3,4),(3,6),(2,6),(2,3),(0,3)]

example5 :: [Pos]
example5 = [(0,0),(8,0),(8,5),(7,5),(7,7),(6,7),(6,1),(4,1),(4,4),(3,4),(3,6),(2,6),(2,3),(0,3)]

-- MAIN LOGIC

parsePos :: String -> [Pos]
parsePos s = map f $ lines s
    where
        f :: String -> Pos
        f s = unlist $ map read $ splitBy ',' s
        splitBy :: Eq a => a -> [a] -> [[a]]
        splitBy x = foldr (\y yss -> if x == y then []:yss else let ys:yss' = yss in (y:ys):yss') [[]]
        unlist :: [a] -> (a, a) -- PRECOND: La lista dada debe tener al menos 2 elementos.
        unlist xs = (head xs, xs !! 1)

biggestArea :: [Pos] -> Int
biggestArea ps = foldr (\p n -> maxArea p ps `max` n) 0 ps
    where
        maxArea :: Pos -> [Pos] -> Int
        maxArea p = foldr (\p' n -> rectangleArea p p' `max` n) 1

rectangleArea :: Pos -> Pos -> Int
rectangleArea (x,y) (x',y') = (max x x' - min x x' + 1) * (max y y' - min y y' + 1)

{-
 0123456789
0----------
1-+---+----
2----------
3-----+--+-
4----------
5----------
6---+----+-
7----------
8-+-+------
9----------
-}

{-
 0123456789
0----------
1-+!!!+----
2-!---!----
3-!---+!!+-
4-!------!-
5-!------!-
6-!-+!!!!+-
7-!-!------
8-+!+------
9----------
-}

{-
 0123456789101112
0........... . .
1.......+!!! + .
2.......!... ! .
3..+!!!!+... ! .
4..!........ ! .
5..+!!!!!!+. ! .
6.........!. ! .
7.........+! + .
8........... . .
-}

biggestInsider :: (Int -> Pos -> Int) -> [(Pos,Pos)] -> (Pos,Pos)
biggestInsider f = head . filter (uncurry (check f)) . orderPos

orderPos :: [(Pos,Pos)] -> [(Pos,Pos)]
orderPos []     = []
orderPos [p]    = [p]
orderPos (p:ps) = (orderPos . larger (uncurry rectangleArea p)) ps ++ [p] ++ (orderPos . smaller (uncurry rectangleArea p)) ps
    where
        smaller :: Int -> [(Pos,Pos)] -> [(Pos,Pos)]
        smaller n = filter (\pl -> uncurry rectangleArea pl <= n)
        larger :: Int -> [(Pos,Pos)] -> [(Pos,Pos)]
        larger  n = filter (\pl -> uncurry rectangleArea pl >= n)

check :: (Int -> Pos -> Int) -> Pos -> Pos -> Bool
check f (x,y) (x',y') = let minX = min x x'
                            maxX = max x x'
                            minY = min y y'
                            maxY = max y y'
                            n    = f maxX (minX,minY)
                                  in all (\p -> f maxX p == n) (pointsToCheck minX (minY+1) maxY)
    where
        pointsToCheck :: Int -> Int -> Int -> [Pos]
        pointsToCheck xx a b = map (xx,) [a..b]

pertenece :: [Pos] -> Pos -> Bool
pertenece ps = odd . intervalosCruzados ps (-1)

intervalosCruzados :: [Pos] -> Int -> Pos -> Int
intervalosCruzados ps maxX p = f maxX p (head ps) ps 0
    where
        f :: Int -> Pos -> Pos -> [Pos] -> Int -> Int
        f _    _  _  []  = id
        f maxX pu po [p]
            | isBetween pu p po 
             && isBefore p maxX = const 1
            | pu == p           
             && isBefore p maxX = const 1
            | isVertical p po 
             && isBefore p maxX = (+ cruzarRayo pu (p,po))
            | otherwise         = id
        f maxX pu po (p:p':ps)
            | isBetween pu p p'   
             && isBefore p maxX   = const 1
            | pu == p || pu == p' 
             && isBefore p maxX   = const 1
            | isVertical   p p'
             && isBefore p maxX   = \n -> f maxX pu po (p':ps) (n + cruzarRayo pu (p,p')) 
            | otherwise           = f maxX pu po (p':ps)
        isVertical :: Pos -> Pos -> Bool
        isVertical (x,_) (x',_) = x == x'
        isBetween :: Pos -> Pos -> Pos -> Bool
        isBetween (xu,yu) (x,y) (_,y') = xu == x && yu > min y y' && yu < max y y'
        isBefore :: Pos -> Int -> Bool
        isBefore _     (-1) = True
        isBefore (x,_) maxX = x < maxX

cruzarRayo :: Pos -> (Pos,Pos) -> Int
cruzarRayo (x,y) (p,p') = fromEnum $ fst p > x && y > apply min snd p p' && y < apply max snd p p'

-- GENERICS

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

apply :: (b -> b -> c) -> (a -> b) -> a -> a -> c
apply f g x y = f (g x) (g y)