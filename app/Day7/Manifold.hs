module Day7.Manifold where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (mapAccumL)

type Pos = (Int, Int)
--          Row  Column

data Cell = Empty | Splitter | Start
    deriving (Eq, Show)

type Grid = M.Map Pos Cell
type Mem = M.Map Pos Integer

parseGrid :: String -> Grid
parseGrid input = M.fromList
        [ ((r, c), parseCell ch)
        | (r, line) <- zip [0..] (lines input)
        , (c, ch)   <- zip [0..] line]
    where
        parseCell :: Char -> Cell
        parseCell '.' = Empty
        parseCell '^' = Splitter
        parseCell 'S' = Start
        parseCell _   = error "invalid cell"

countSplits :: Grid -> Int
countSplits g = f S.empty [findStart g]
    where
        f :: S.Set Pos -> [Pos] -> Int
        f _       []     = 0
        f visited (p:ps)
            | p `S.member` visited = f visited ps
            | otherwise            = let visited' = S.insert p visited
                                         next     = nextPositions g p
                                         here     = case M.lookup p g of
                                                    Just Splitter -> 1
                                                    _             -> 0
                                     in here + f visited' (next ++ ps)

countPaths :: Grid -> Integer
countPaths g = fst (pathsFrom M.empty (findStart g))
    where
        pathsFrom :: Mem -> Pos -> (Integer, Mem)
        pathsFrom mem p
            | fst p > maxRow           = (1, mem)
            | Just v <- M.lookup p mem = (v, mem)
            | otherwise                = let next         = nextPositions g p
                                             (mem', vals) = mapAccumL (\m q -> let (v, m') = pathsFrom m q in (m', v)) mem next
                                             total        = sum vals
                                         in (total, M.insert p total mem')
        maxRow :: Int
        maxRow = maximum [r | ((r,_), _) <- M.toList g]

-- ## -------------------------------------- aux ---------------------------- ## --
nextPositions :: Grid -> Pos -> [Pos]
nextPositions g (r, c) = case M.lookup (r, c) g of
                         Just Empty    -> [(r+1, c)]
                         Just Start    -> [(r+1, c)]
                         Just Splitter -> [ (r+1, c-1)
                                          , (r+1, c+1)]
                         Nothing       -> []  -- fell out of the grid

findStart :: Grid -> Pos
findStart g = head [p | (p, Start) <- M.toList g]