module Day1.Dial (Dial, evalDial, evalSequence, method0x434C49434B, readDials, count0s) where 

data Dial = L Int
          | R Int
          deriving (Show)

instance Read Dial where
    readsPrec _ s = case s of
            ('L':rest) -> [(L n, rest') | (n, rest') <- reads rest]
            ('R':rest) -> [(R n, rest') | (n, rest') <- reads rest]
            _          -> []

-- Me dan una posición inicial y un Dial y devuelvo la posición resultante
evalDial :: Int -> Dial -> Int
evalDial n (L n') = evalDialL n n'
evalDial n (R n') = evalDialR n n'

-- Me dan una posición inicial, una lista de Diales y devuelvo una lista con las posiciones intermedias
evalSequence :: Int -> [Dial] -> [Int]
evalSequence = flip (foldr (\d f n -> let m = evalDial n d
                                      in m : f m) (const []))

-- Me dan una posición inicial, una lista de Diales y devuelvo la cantidad de veces que pasa alguna vez por 0.
method0x434C49434B :: Int -> [Dial] -> Int
method0x434C49434B = flip (foldr (\d f n -> let m = evalDial n d
                                            in g n d + f m) (const 0))
    where g n (L n') = sumLPasses n n' 0
          g n (R n') = sumRPasses n n' 0

readDials :: String -> [Dial]
readDials xs = map read (lines xs)

count0s :: [Int] -> Int
count0s = foldr (\n m -> fromEnum (n == 0) + m) 0

-- aux
evalDialL :: Int -> Int -> Int
evalDialL 0 m = case m of
                0 -> 0
                _ -> evalDialL 99 (m-1)
evalDialL n m = case m of
                0 -> n
                _ -> evalDialL (n-1) (m-1)

evalDialR :: Int -> Int -> Int
evalDialR 99 m = case m of
                 0 -> 99
                 _ -> evalDialR 0 (m-1)
evalDialR n  m = case m of
                 0 -> n
                 _ -> evalDialR (n+1) (m-1)

sumLPasses :: Int -> Int -> Int -> Int 
sumLPasses 0 m = \r -> case m of
                       0 -> r
                       _ -> sumLPasses 99 (m-1) r
sumLPasses 1 m = \r -> case m of
                       0 -> r
                       _ -> sumLPasses 0 (m-1) (r+1)
sumLPasses n m = \r -> case m of
                       0 -> r
                       _ -> sumLPasses (n-1) (m-1) r

sumRPasses :: Int -> Int -> Int -> Int
sumRPasses 99 m = \r -> case m of
                        0 -> r
                        _ -> sumRPasses 0 (m-1) (r+1)
sumRPasses n  m = \r -> case m of
                        0 -> r
                        _ -> sumRPasses (n+1) (m-1) r