module Day5.Sets (readRanges, getSetFromRanges, getIntervalFromRanges, evalAll, countFreshIngredients) where
import Data.Char

data Set a = Union (Set a) (Set a)
           | Inter (Set a) (Set a)
           | Lit (a -> Bool)
           | EmptyS

type Interval = [Range]

data Range = R Int Int
            deriving (Show, Eq)

instance Read Range where
    readsPrec _ s =
        [(R (read a) (read b), rest') |
           (a, '-' : rest) <- [span isDigit s],
           not (null a),
           (b, rest') <- [span isDigit rest],
           not (null b)]

readRanges :: [String] -> [Range]
readRanges = map read

getSetFromRanges :: [Range] -> Set Int
getSetFromRanges = foldr (Union . evalR) EmptyS
    where
        evalR :: Range -> Set Int
        evalR (R n m) = Inter (Lit (>=n)) (Lit (<= m))

getIntervalFromRanges :: [Range] -> Interval
getIntervalFromRanges = foldr joinIntervals []

evalAll :: [a] -> Set a -> Int
evalAll = foldr (\x f s -> fromEnum (evalSet x s) + f s) (const 0)

countFreshIngredients :: Interval -> Int
countFreshIngredients = foldr (\(R mn mx) n -> (mx - mn + 1) + n) 0

-- ## ------------------------------------- aux ----------------------------------- ## --
foldSet :: b -> ((a -> Bool) -> b) -> (b -> b -> b) -> (b -> b -> b) -> Set a -> b
foldSet z _ _ _ EmptyS        = z
foldSet _ f _ _ (Lit p)       = f p
foldSet z f g h (Union s1 s2) = g (foldSet z f g h s1) (foldSet z f g h s2)
foldSet z f g h (Inter s1 s2) = h (foldSet z f g h s1) (foldSet z f g h s2)

evalSet :: a -> Set a -> Bool
evalSet x = foldSet False (\p -> p x) (||) (&&)

joinIntervals :: Range -> Interval -> Interval
joinIntervals r           []      = [r]
joinIntervals r@(R mn mx) (r':rs) = case r' of
                                    R mn' mx' -> if not (overlaps r r')
                                                 then r' : joinIntervals r rs
                                                 else let tr = R (min mn mn') (max mx mx')
                                                      in if canBeJoined tr rs
                                                         then joinIntervals tr rs
                                                         else tr : rs

canBeJoined :: Range -> Interval -> Bool
canBeJoined _ []      = False
canBeJoined r (r':rs) = overlaps r r' || canBeJoined r rs

overlaps :: Range -> Range -> Bool
overlaps (R mn mx) (R mn' mx') = min mx mx' >= max mn mn'