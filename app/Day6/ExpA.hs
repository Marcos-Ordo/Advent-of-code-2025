module Day6.ExpA (readExpA, readExpA', totalNumber) where
import Data.Char
import Data.List

data ExpA = Lit Int
          | Sum ExpA ExpA
          | Mul ExpA ExpA
          deriving Show

instance Read ExpA where
  readsPrec _ s =
    let nums = extractInts s
        expr
          | '*' `elem` s = foldMul nums
          | '+' `elem` s = foldSum nums
          | otherwise    =
              case nums of
                [n] -> Lit n
                _   -> error "No se encontró un operador en la lista"
    in [(expr, "")]

readExpA :: String -> [ExpA]
readExpA = map read . foldr (joinBy (\s1 s2 -> s1 ++ " " ++ s2) . words) [] . lines

readExpA' :: String -> [ExpA]
readExpA' ss = let (x,y) = splitAt (length (map words (normalize ss)) - 1) (map words (normalize ss))
               in map read (joinBy (\s1 s2 -> s1 ++ " " ++ s2) (map (concatMap (++" ")) (appendToEmpty x)) (head y))

totalNumber :: [ExpA] -> Int
totalNumber = foldr (\e n -> evalExpA e + n) 0

-- ## ---------------------------- aux --------------------------------- ## --
extractInts :: String -> [Int]
extractInts = map read . words . map (\c -> if isDigit c then c else ' ')

recr :: ([t1] -> t1 -> t2 -> t2) -> t2 -> [t1] -> t2
recr _ z []     = z
recr f z (x:xs) = f xs x (recr f z xs)

foldExpA :: (Int -> t) -> (t -> t -> t) -> (t -> t -> t) -> ExpA -> t
foldExpA f _ _ (Lit n)     = f n
foldExpA f g h (Sum e1 e2) = g (foldExpA f g h e1) (foldExpA f g h e2)
foldExpA f g h (Mul e1 e2) = h (foldExpA f g h e1) (foldExpA f g h e2)

foldMul :: [Int] -> ExpA
-- PRECOND: La lista dada debe tener al menos un número!
foldMul = recr (\ns n e -> case ns of
                           [] -> Lit n
                           _  -> Mul (Lit n) e) (error "La lista dada debe tener al menos un número!")

foldSum :: [Int] -> ExpA
-- PRECOND: La lista dada debe tener al menos un número!
foldSum = recr (\ns n e -> case ns of
                           [] -> Lit n
                           _  -> Sum (Lit n) e) (error "La lista dada debe tener al menos un número!")

joinBy :: (String -> String -> String) -> [String] -> [String] -> [String]
joinBy _ []       yss      = yss
joinBy _ xss      []       = xss
joinBy f (xs:xss) (ys:yss) = f xs ys : joinBy f xss yss

evalExpA :: ExpA -> Int
evalExpA = foldExpA id (+) (*)

normalize :: String -> [String]
normalize s = let l      = lines s
                  (ws,d) = splitAt (length l-1) l
              in transpose ws ++ d
              
appendToEmpty :: [[String]] -> [[String]]
appendToEmpty ss = f ss [] []
    where
        f :: [[String]] -> [String] -> [[String]] -> [[String]]
        f ([]:ss') rs nrs
            | null rs    = f ss' [] nrs
            | otherwise  = f ss' [] (nrs ++ [rs])
        f (s:ss') rs nrs = f ss' (rs ++ s) nrs
        f [] rs nrs
            | null rs    = nrs
            | otherwise  = nrs ++ [rs]