module Answers where

import Day1.Dial
import Day2.IdRanges
import Day3.BatteryBank
import Day4.PaperRolls
import Day5.Sets
import Day6.ExpA
import Day7.Manifold

answerDay1 :: IO ()
answerDay1 = do ss <- readFile "./app/Day1/input.txt"
                print (count0s (evalSequence 50 (readDials ss)))
                print (method0x434C49434B 50 (readDials ss))

answerDay2 :: IO ()
answerDay2 = do ss <- readFile "./app/Day2/input.txt"
                print (countElfDoingsInRanges (readIdRanges ss))

answerDay3 :: IO ()
answerDay3 = do ss <- readFile "./app/Day3/input.txt"
                print (totalOutput 2 (readBank ss))
                print (totalOutput 12 (readBank ss))

answerDay4 :: IO ()
answerDay4 = do ss <- readFile "./app/Day4/input.txt"
                let pa = readPaperArea ss
                 in do print (contarPaperRollDisponibles pa)
                       print (contarPaperRollsBorrados pa)

answerDay5 :: IO ()
answerDay5 = do ss <- readFile "./app/Day5/input.txt"
                let (ss',ns) = break null (lines ss)
                    ns'     = map read (tail ns) :: [Int]
                    s       = getSetFromRanges (readRanges ss')
                    i       = getIntervalFromRanges (readRanges ss')
                 in do print (evalAll ns' s)
                       print (countFreshIngredients i)

answerDay6 :: IO ()
answerDay6 = do ss <- readFile "./app/Day6/input.txt"
                print (totalNumber (readExpA ss))
                print (totalNumber (readExpA' ss))

answerDay7 :: IO ()
answerDay7 = do s <- readFile "./app/Day7/input.txt"
                print (countSplits (parseGrid s))
                print (countPaths  (parseGrid s))