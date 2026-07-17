{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use list comprehension" #-}

module Day9.Theather where

type Pos = (Int,Int) -- (Eje X, Eje Y)
data Intervalo = I Int (Int -> Bool) -- (Eje Y / nivel) (Intervalo X)

getBiggestArea :: String -> Int
getBiggestArea = biggestArea . parsePos

getBiggestInsider :: String -> Int
getBiggestInsider s = let ps = parsePos s
                          f  = pertenece ps
                       in biggestInsider2 (odd . f) ps

-- EXAMPLES

textExample :: String
textExample = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"

example :: [Pos]
example = [(7,1),(11,1),(11,7),(9,7),(9,5),(2,5),(2,3),(7,3)]

example2 :: [Pos]
example2 = [(1,1),(5,1),(5,3),(8,3),(3,6),(8,6),(1,8),(3,8)]

example3 :: [Pos]
example3 = [(7,1),(11,1),(2,3),(7,3),(2,5),(9,5),(9,7),(11,7)]

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

biggestInsider2 :: (Pos -> Bool) -> [Pos] -> Int
biggestInsider2 f ps = foldr (\p n -> maxArea p f ps `max` n) 1 ps
    where
        maxArea :: Pos -> (Pos -> Bool) -> [Pos] -> Int
        maxArea p f = foldr (\p' n -> if checkPerimeter f p p' then rectangleArea p p' `max` n else n) 1
        rectangleArea :: Pos -> Pos -> Int
        rectangleArea (x,y) (x',y') = (max x x' - min x x' + 1) * (max y y' - min y y' + 1)

checkPerimeter :: (Pos -> Bool) -> Pos -> Pos -> Bool
checkPerimeter f (x,y) (x',y') = all f (g (min x x') (max x x') (min y y') (max y y'))
    where
        g :: Int -> Int -> Int -> Int -> [Pos]
        g minX maxX minY maxY = weirdZip minY [minX..maxX] maxY [minX..maxX] minX [minY..maxY] maxX [minY..maxY]
        
weirdZip :: Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [Pos]
weirdZip minY []       maxY []       minX []       maxX []       = []
weirdZip minY []       maxY []       minX []       maxX (mY:mYs) = (maxX,mY) : weirdZip minY []  maxY []  minX []  maxX mYs
weirdZip minY []       maxY []       minX (mY:mYs) maxX mys      = (minX,mY) : weirdZip minY []  maxY []  minX mYs maxX mys
weirdZip minY []       maxY (mX:mXs) minX mYs      maxX mys      = (mX,maxY) : weirdZip minY []  maxY mXs minX mYs maxX mys
weirdZip minY (mX:mXs) maxY mxs      minX mYs      maxX mys      = (mX,minY) : weirdZip minY mXs maxY mxs minX mYs maxX mys

pertenece :: [Pos] -> Pos -> Int
pertenece ps p = f p (head ps) ps 0
    where
        f :: Pos -> Pos -> [Pos] -> Int -> Int
        f _  _  []  n = n
        f pu po [p] n
            | isBetween pu p po = 1
            | pu == p           = 1
            | isVertical   p po = n + cruzarRayo pu (p,po)
            | otherwise         = n
        f pu po (p:p':ps) n
            | isBetween pu p p' = 1
            | isAPoint  pu p p' = 1
            | isVertical   p p' = f pu po (p':ps) (n + cruzarRayo pu (p,p'))
            | otherwise         = f pu po (p':ps) n
        isVertical :: Pos -> Pos -> Bool
        isVertical (x,_) (x',_) = x == x'
        isBetween :: Pos -> Pos -> Pos -> Bool
        isBetween (xu,yu) (x,y) (x',y') = xu == x && yu > y && yu < y'
        isAPoint :: Pos -> Pos -> Pos -> Bool
        isAPoint pu p p' = pu == p || pu == p'

cruzarRayo :: Pos -> (Pos,Pos) -> Int
cruzarRayo (x,y) (p,p') = fromEnum $ fst p > x && y > apply min snd p p' && y < apply max snd p p'

apply :: (b -> b -> c) -> (a -> b) -> a -> a -> c
apply f g x y = f (g x) (g y)