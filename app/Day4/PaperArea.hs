module Day4.PaperArea where

{- Inv. Rep.:
 - * dada una Grid 'g' con listas de listas de Celdas 'fs', todas las 'fs' de 'g' deben tener la misma longitud.
 - * dada una Grid 'g' con listas de listas de Celdas 'fs', para cada lista de celda 'cs' de cada 'fs', deben
     tener la misma longitud.
 - * dada una Grid 'g' con un par de Enteros 'p', ambos números deben ser mayores o iguales a 0.
 -}

data PaperArea = Grid [[Celda]] (Int, Int)
                deriving Show
                --    Filas      Punto
                --     Columnas   Y    x
data Celda = Papel Bool | Nada Bool
            deriving Show

data Dir = Norte | Este | Sur | Oeste
          deriving Show

areaInicial :: [String] -> PaperArea
-- PRECOND: Cada caracter de cada string debe ser válido.
areaInicial ss = Grid (f ss) (0,0)
    where
        f :: [String] -> [[Celda]]
        f = map g
        g :: String -> [Celda]
        g = foldr (\c cs -> if c == '@'
                            then Papel False : cs
                            else if c == '.'
                                 then Nada False : cs
                                 else error "areaInicial: Los caractéres admitidos son @ y .") []

mover :: Dir -> PaperArea -> PaperArea
-- PRECOND: La dirección objetivo debe ser posible en el area dada.
mover Este  pa@(Grid fs p) = let (y,x) = p
                             in if puedeMover Este pa
                                then Grid fs (y,x+1)
                                else error "mover: Me pase y me caí de la grilla por el Este!"
mover Sur   pa@(Grid fs p) = let (y,x) = p
                             in if puedeMover Sur pa
                                then Grid fs (y+1,x)
                                else error "mover: Me pase y me caí de la grilla por el Sur!"
mover Norte pa@(Grid fs p) = let (y,x) = p
                             in if puedeMover Norte pa
                                then Grid fs (y-1,x)
                                else error "mover: Me pase y me caí de la grilla por el Norte!"
mover Oeste pa@(Grid fs p) = let (y,x) = p
                             in if puedeMover Oeste pa
                                then Grid fs (y,x-1)
                                else error "mover: Me pase y me caí de la grilla por el Oeste!"

puedeMover :: Dir -> PaperArea -> Bool
puedeMover Este  (Grid fs p) = let (y,x) = p in x+1 < length (fs !! y)
puedeMover Sur   (Grid fs p) = let (y,_) = p in y+1 < length fs
puedeMover Norte (Grid _  p) = let (y,_) = p in y > 0
puedeMover Oeste (Grid _  p) = let (_,x) = p in x > 0

hayPaperRoll :: PaperArea -> Bool
hayPaperRoll (Grid fs p) = let (y,x) = p in esPapel ((fs !! y) !! x)
    where
        esPapel :: Celda -> Bool
        esPapel (Papel _) = True
        esPapel _         = False

sacarMarcados :: PaperArea -> PaperArea
sacarMarcados (Grid fs p) = Grid (f fs) p
    where
        f :: [[Celda]] -> [[Celda]]
        f = map g
        g :: [Celda] -> [Celda]
        g = foldr (\c cs -> if estáMarcado c then Nada False : cs else c : cs) []

marcar :: PaperArea -> PaperArea
marcar (Grid fs p) = let (y,x) = p in Grid (replaceNth y (replaceNth x (marcar' ((fs !! y) !! x)) (fs !! y)) fs) p
    where
        marcar' :: Celda -> Celda
        marcar' (Papel _) = Papel True
        marcar' (Nada  _) = Nada True

-- ## --------------------------- aux ----------------------------- ## --
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x xs
    | n < 0     = xs
    | otherwise = take n xs ++ [x] ++ drop (n + 1) xs


estáMarcado :: Celda -> Bool
estáMarcado (Papel b) = b
estáMarcado (Nada  b) = b