module Answers where

import Day1.Dial
import Day2.IdRanges
import Day3.BatteryBank
import Day4.PaperRolls

answerDay1 :: IO ()
answerDay1 = do ss <- readFile "./app/Day1/input.txt"
                print (count0s (evalSequence 50 (readDials ss)))
                print (method0x434C49434B 50 (readDials ss))

answerDay2 :: IO ()
answerDay2 = do ss <- readFile "./app/Day2/input.txt"
                print (countElfDoingsInRanges (readRanges ss))

answerDay3 :: IO ()
answerDay3 = do ss <- readFile "./app/Day3/input.txt"
                print (totalOutput 2 (readBank ss))
                print (totalOutput 12 (readBank ss))

answerDay4 :: IO ()
answerDay4 = do ss <- readFile "./app/Day4/input.txt"
                let pa = readPaperArea ss
                 in do print (contarPaperRollDisponibles pa)
                       print (contarPaperRollsBorrados pa)