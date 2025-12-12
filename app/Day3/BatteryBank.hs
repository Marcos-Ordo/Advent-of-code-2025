module Day3.BatteryBank (BatteryBank, Battery, totalOutput, readBank) where
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

-- ## -------------------------- Main -------------------------------- ## --
totalOutput :: Int -> BatteryBank -> Int
totalOutput n bk = (sum . map formNumber) (maxOutageForBank n bk)

maxOutageForBank :: Int -> BatteryBank -> [[Int]]
maxOutageForBank = map . maxOutageWithNBatteries

maxOutageWithNBatteries :: Int -> Battery -> [Int]
maxOutageWithNBatteries n b = f n b []
    where
        f :: Int -> Battery -> [Int] -> [Int]
        f = flip (foldBattery (\_ rs -> rs) (\n' g nb rs -> fall nb n' (g nb rs)))

readBank :: String -> BatteryBank
readBank s = map read (lines s)

-- ## -------------------------- Aux ----------------------------- ## --
fall :: Int -> Int -> [Int] -> [Int]
fall nb n [] = [n | nb /= 0]
fall nb n ts = if nb == length ts
               then replace n ts
               else n : ts

replace :: Int -> [Int] -> [Int]
replace _ []     = []
replace n (m:ms) = if n >= m then n : replace m ms else m : ms

formNumber :: [Int] -> Int
-- PRECOND: Tiene que haber al menos un numero para poder formarlo!
formNumber []     = error "formNumber: Tiene que haber al menos un numero para poder formarlo!"
formNumber [n]    = n
formNumber (n:ns) = read (show n ++ show (formNumber ns))