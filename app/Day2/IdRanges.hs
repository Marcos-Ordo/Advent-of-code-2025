module Day2.IdRanges (IdRange, evalIdRange, countElfDoingsInRange, countElfDoingsInRanges, readRanges) where
import Data.Char (isDigit)

data IdRange = IdRange Int Int
    deriving Show

instance Read IdRange where
    readsPrec _ s =
        [(IdRange (read a) (read b), rest') |
           (a, '-' : rest) <- [span isDigit s],
           not (null a),
           (b, rest') <- [span isDigit rest],
           not (null b)]

evalIdRange :: IdRange -> [Int]
evalIdRange (IdRange n m) = [n..m]

countElfDoingsInRanges :: [IdRange] -> Int
countElfDoingsInRanges = foldr (\i nr -> countElfDoingsInRange i + nr) 0

countElfDoingsInRange :: IdRange -> Int
countElfDoingsInRange = countElfDoings . evalIdRange

readRanges :: String -> [IdRange]
readRanges s = map read (commas s)

-- ## --------------------------- aux ------------------------------ ## --
countElfDoings :: [Int] -> Int
countElfDoings = foldr (\n nr -> if isElfDoing n then n + nr else nr) 0

isElfDoing :: Int -> Bool
isElfDoing n = f (show n) m
    where m = (length . show) n
          f _ 1  = False
          f s n' = let ss = splitBy (div (length s) n') s
                   in case ss of
                      []       -> False
                      (s':ss') -> all (==s') ss' || f s (n'-1)

splitBy :: Int -> [a] -> [[a]]
-- PRECOND: El número dado no puede ser 0!
splitBy 0 _  = error "splitBy: El número dado no puede ser 0!"
splitBy _ [] = []
splitBy n xs = uncurry (:) (case breakN n xs of
                            (zs,zs') -> (zs, splitBy n zs'))

breakN :: Int -> [a] -> ([a],[a])
breakN _ xs@[]      = (xs, xs)
breakN n xs@(x:xs')
        | n == 0    = ([],xs)
        | otherwise = let (ys,zs) = breakN (n-1) xs' in (x:ys,zs)

commas :: String -> [String]
commas "" = []
commas s  = uncurry (:) (case break (==',') s of
                         (l,s') -> (l, case s' of
                                       []    -> []
                                       _:s'' -> commas s''))
-- ## --------------------------- aux ------------------------------ ## --
