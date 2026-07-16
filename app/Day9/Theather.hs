{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Day9.Theather where
import Data.Ord
import Data.List

type Pos = (Int,Int) -- (Eje X, Eje Y)
data Intervalo = I Int (Int -> Bool) -- (Eje Y / nivel) (Intervalo X)

getBiggestArea :: String -> Int
getBiggestArea = biggestArea . parsePos

getBiggestInsider :: String -> Int
getBiggestInsider s = let ps = parsePos s
                          is = makeIntervals $ makePairs $ orderPos ps
                       in biggestInsider is ps

-- TESTS

liveTest :: IO ()
liveTest = do s <- readFile "./app/Day9/input.txt"
              print $ maxPosWithSameY $ orderPos $ parsePos s
    where
        maxPosWithSameY :: [Pos] -> Int
        maxPosWithSameY []     = 0
        maxPosWithSameY (p:ps) = f p ps 1
        f :: Pos -> [Pos] -> Int -> Int
        f p []      n = n
        f p (p':ps) n = if snd p == snd p' then max (n+1) (f p ps (n+1)) else max n (f p' ps 1)

test :: Int
test = getBiggestArea textExample

test2 :: Pos -> [Pos] -> Bool
test2 p ps = belongsIntervals p (makeIntervals $ makePairs $ orderPos ps)

test3 :: [Pos] -> [(Pos, Pos)]
test3 ps = g ps (makeIntervals $ makePairs $ orderPos ps)
    where
        g :: [Pos] -> [Intervalo] -> [(Pos, Pos)]
        g ps is = filter (\(a,b) -> not (f is a b)) [(a,b) | a <- ps, b <- ps]
        f :: [Intervalo] -> Pos -> Pos -> Bool
        f is (x,y) (x',y')
            | y == y' = True
            | x == x' = True
            | (y < y' && x < x') || (y > y' && x > x') = belongsIntervals (x',y) is && belongsIntervals (x,y') is
            | (y < y' && x > x') || (y > y' && x < x') = belongsIntervals (x,y') is && belongsIntervals (x',y) is

test4 :: [Pos] -> ((Pos, Pos), Int)
test4 ps = g ps (makeIntervals $ makePairs $ orderPos ps)
    where
        g ps is = maximumBy (comparing snd) [ ((p,p'), rectangleArea p p')
                                            | p <- ps
                                            , p' <- ps
                                            , f is p p']
        f :: [Intervalo] -> Pos -> Pos -> Bool
        f is (x,y) (x',y')
            | y == y' = True
            | x == x' = True
            | (y < y' && x < x') || (y > y' && x > x') = belongsIntervals (x',y) is && belongsIntervals (x,y') is
            | (y < y' && x > x') || (y > y' && x < x') = belongsIntervals (x,y') is && belongsIntervals (x',y) is
        rectangleArea :: Pos -> Pos -> Int
        rectangleArea (x,y) (x',y') = (max x x' - min x x' + 1) * (max y y' - min y y' + 1)

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

{- PARA la parte 2, Creo que tengo que crear el contorno de tiles verdes como un conjunto, que luego, con cualquier Pos (tiles rojas) puedo saber si está dentro o fuera del area permitida-}

orderPos :: [Pos] -> [Pos]
orderPos []     = []
orderPos [p]    = [p]
orderPos (p:ps) = (orderPos . smaller p) ps ++ [p] ++ (orderPos . larger p) ps
    where
        smaller :: Pos -> [Pos] -> [Pos]
        smaller (_,y) = orderByX . filter (\(_,y') -> y' < y)
        larger :: Pos -> [Pos] -> [Pos]
        larger (_,y) = orderByX . filter (\(_,y') -> y' >= y)

orderByX :: [Pos] -> [Pos]
orderByX []     = []
orderByX [p]    = [p]
orderByX (p:ps) = (orderByX . smaller p) ps ++ [p] ++ (orderByX . larger p) ps
    where
        smaller :: Pos -> [Pos] -> [Pos]
        smaller (x,_) = filter (\(x',_) -> x' < x)
        larger :: Pos -> [Pos] -> [Pos]
        larger (x,_) = filter (\(x',_) -> x' >= x)

makePairs :: [Pos] -> [(Pos,Pos)]
makePairs = g . f . makeOffset
    where
        g :: [(Pos,Pos)] -> [(Pos,Pos)]
        g []        = []
        g [p]       = [p]
        g (p:p':ps) = insiders p p' ++ g (p':ps)
        f :: [(Pos,Pos,Int)] -> [(Pos,Pos)]
        f []            = []
        f ((p,p',n):ps) = if n > 0
                          then (p,applyOffset p' n): f ps
                          else (applyOffset p n,p'): f ps
        applyOffset :: Pos -> Int -> Pos
        applyOffset (x,y) n = (x+n,y)

insiders :: (Pos,Pos) -> (Pos,Pos) -> [(Pos,Pos)]
insiders (p,p') (pr,pr') = (p,p') : f (1 + snd p) (snd pr) (p,p') (pr,pr')
    where
        f y y' (p,p') (pr,pr')
            | y == y' = []
            | y <  y' = if fst p == fst pr
                        then ((fst p, y),(min (fst p') (fst pr'), y)) : f (y+1) y' (p,p') (pr,pr')
                        else ((max (fst p) (fst pr), y),(fst p', y)) : f (y+1) y' (p,p') (pr,pr')

makeOffset :: [Pos] -> [(Pos,Pos,Int)]
makeOffset []        = []
makeOffset [p, p']   = [(p,p',0)]
makeOffset (p:p':ps) = (f . makeOffset) ps p p'
    where
        f :: [(Pos,Pos,Int)] -> Pos -> Pos -> [(Pos,Pos,Int)]
        f []                 _ _  = error "No debería haber pasado ..."
        f ps'@((pr,pr',n):_) p p'
            | n <= 0 && fst p == fst pr' = (p,p',offset pr p + n) : ps'
            | n > 0  && fst p == fst pr' = (p,p',offset pr' p' + n) : ps'
            | n >= 0 && fst p' == fst pr = (p,p',offset pr' p' + n) : ps'
            | n < 0  && fst p' == fst pr = (p,p',offset pr p + n) : ps'
            | fst p  == fst pr  = (p,p',offset pr' p' + n) : ps'
            | fst p' == fst pr' = (p,p',offset pr p + n) : ps'
            | otherwise = error $ "No debería haber pasado ... " ++ show ps' ++ " " ++ show p ++ " " ++ show p'
        offset :: Pos -> Pos -> Int
        offset (x,_) (x',_) = x - x'

makeIntervals :: [(Pos,Pos)] -> [Intervalo]
makeIntervals = map f
    where
        f :: (Pos,Pos) -> Intervalo
        f ((x, y), (x', _)) = I y (\x'' -> x'' >= x && x'' <= x')

belongsIntervals :: Pos -> [Intervalo] -> Bool
belongsIntervals p = any (belongsIn p)
    where
        belongsIn :: Pos -> Intervalo -> Bool
        belongsIn (x,y) (I y' f) = y == y' && f x

biggestInsider :: [Intervalo] -> [Pos] -> Int
biggestInsider is ps = foldr (\p n -> maxArea p is ps `max` n) 1 ps
    where
        maxArea :: Pos -> [Intervalo] -> [Pos] -> Int
        maxArea p is = foldr (\p' n -> if f is p p' then rectangleArea p p' `max` n else n) 1
        rectangleArea :: Pos -> Pos -> Int
        rectangleArea (x,y) (x',y') = (max x x' - min x x' + 1) * (max y y' - min y y' + 1)
        f :: [Intervalo] -> Pos -> Pos -> Bool
        f is (x,y) (x',y')
            | y == y' = True
            | x == x' = True
            | (y < y' && x < x') || (y > y' && x > x') = belongsIntervals (x',y) is && belongsIntervals (x,y') is
            | (y < y' && x > x') || (y > y' && x < x') = belongsIntervals (x,y') is && belongsIntervals (x',y) is
            | otherwise = False

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