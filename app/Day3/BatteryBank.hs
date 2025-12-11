module Day3.BatteryBank where
import Data.Char (isDigit, digitToInt)

type BatteryBank = [Battery]

data Battery = NoJolts
             | Jolt Int Battery
             deriving Show

instance Read Battery where
    readsPrec _ s =
        let digits = takeWhile isDigit s
            rest   = dropWhile isDigit s
        in if null digits
           then [(NoJolts, s)] 
           else [(fromDigits digits, rest)]
      where
        fromDigits :: String -> Battery
        fromDigits []     = NoJolts
        fromDigits (c:cs) = Jolt (digitToInt c) (fromDigits cs)

foldBattery :: b -> (Int -> b -> b) -> Battery -> b
foldBattery z _ NoJolts    = z
foldBattery z f (Jolt n b) = f n (foldBattery z f b)

recBattery :: b -> (Battery -> Int -> b -> b) -> Battery -> b
recBattery z _ NoJolts    = z
recBattery z f (Jolt n b) = f b n (recBattery z f b)

-- ## -------------------------- Main -------------------------------- ## --

maxOutage :: Battery -> (Int, Int)
maxOutage = undefined

readBank :: String -> BatteryBank
readBank s = map read (lines s)