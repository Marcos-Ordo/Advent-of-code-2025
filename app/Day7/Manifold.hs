module Day7.Manifold where

data Manifold = Splitter Manifold Manifold
              | Laser Manifold
              | EManifold

--readManifold :: [String] -> Manifold
readManifold ss = readManifold' (filter (not . isLineOfDots) ss)

readManifold' []     = EManifold
readManifold' (s:ss) = let (manI,manD) = appPar (map (splitAt (positionOf (=='S') s)) ss)
                       in Splitter (readLeftManifold (tail manI)) (readRightManifold (tail manD))

readLeftManifold :: [String] -> Manifold
readLeftManifold [] = EManifold
readLeftManifold ss = let (manI',manD') = appPar (map splitHead ss)
                      in undefined

readRightManifold = undefined

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

appPar :: [(a,b)] -> ([a],[b])
appPar []     = ([],[])
appPar (p:ps) = let (x, y)  = p
                    (xs,ys) = appPar ps
                in  (x:xs,y:ys)

splitHead :: [a] -> (a,[a])
-- PRECOND: La lista debe tener al menos un elemento
splitHead []     = error "splitHead: La lista debe tener al menos un elemento"
splitHead (x:xs) = (x,xs) 

isLineOfDots :: String -> Bool
isLineOfDots = all (=='.')