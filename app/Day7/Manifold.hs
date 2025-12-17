module Day7.Manifold where

data Manifold = Splitter Manifold Manifold
              | Laser Manifold
              | EmptyM
              deriving (Show, Eq)

readManifold :: [String] -> Manifold
readManifold ss = readManifold' (filter (not . isLineOfDots) ss)

readManifold' :: [String] -> Manifold
readManifold' []     = EmptyM
readManifold' (s:ss) = evalLaser (positionOf (=='S') s) ss

evalLaser :: Int -> [String] -> Manifold
evalLaser _ []     = EmptyM
evalLaser n (s:ss) = searchM (s !! n) n ss

evalSplitter :: Int -> Int -> [String] -> (Manifold, Manifold)
evalSplitter _ _ []     = (EmptyM,EmptyM)
evalSplitter n m (s:ss) = (searchM (s !! n) n ss, searchM (s !! m) m ss)

searchM :: Char -> Int -> [String] -> Manifold
searchM '^' n ss = let (m1, m2) = evalSplitter (n-1) (n+1) ss in Splitter m1 m2
searchM '.' n ss = Laser (evalLaser n ss)
searchM _   _ _  = error "searchM: caracter equivocado!"


{- Simplifico bajo las siguientes reglas:
    * Laser EmptyM = EmptyM
    * Splitter (Splitter mii mid) (Splitter mdi mdd) #donde mid == mdi# = Splitter (Splitter mii mid) (Splitter EmptyM mdd)
-}
simplify :: Manifold -> Manifold
simplify EmptyM           = EmptyM
simplify (Laser m)        = simplifyLaser (simplify m)
simplify (Splitter m1 m2) = simplifySplitter (simplify m1) (simplify m2)

simplifySplitter :: Manifold -> Manifold -> Manifold
simplifySplitter m1@(Splitter _ mid) m2 = case m2 of
                                          Splitter mdi mdd -> if mid == mdi
                                                              then Splitter m1 (Splitter EmptyM mdd)
                                                              else Splitter m1 m2
                                          _                -> Splitter m1 m2
simplifySplitter m1                  m2 = Splitter m1 m2

simplifyLaser :: Manifold -> Manifold
simplifyLaser EmptyM = EmptyM
simplifyLaser m      = Laser m

-- Cumple la siguiente propiedad:
-- positionOf (const False) xs = length xs
{- Demo de esa propiedad:
    para todo xs. ¿positionOf (const False) xs = length xs?
    sea xs una lista de elementos cualquiera, es equivalente demostrar:
    positionOf (const False) xs = length xs
    por ppio d ind. estr. sobre xs, es equivalente demostrar:

    Caso Base, xs -> [])
        ¿positionOf (const False) [] = length []?
    
    Caso Inductivo, xs -> (x:xs'))
        (HI) ¡positionOf (const False) xs' = length xs'!
        (TI) ¿positionOf (const False) (x:xs') = length (x:xs')?

    ahora voy a demostrar por los casos dados:

    Caso Base)
        Lado Izq
            positionOf (const False) []
        =                                   (positionOf)
            0
        
        Lado Der
            length []
        =                                   (length)
            0
    
    Caso Inductivo)
        Lado Izq
            positionOf (const False) (x:xs')
        =                                   (positionOf)
            if const False x
            then 0
            else 1 + positionOf (const False) xs'
        =                                   (const)
            if False
            then 0
            else 1 + positionOf (const False) xs'
        =                                   (if-then-else)
            1 + positionOf (const False) xs'
        =                                   (HI)
            1 + length xs'

        Lado Der
            length (x:xs')
        =                                   (length)
            1 + length xs'
-}
positionOf :: (a -> Bool) -> [a] -> Int
positionOf _ []     = 0
positionOf p (x:xs) = if p x then 0 else 1 + positionOf p xs

isLineOfDots :: String -> Bool
isLineOfDots = all (=='.')