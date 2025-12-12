module Day4.PaperRolls (readPaperArea, contarPaperRollDisponibles, contarPaperRollsBorrados) where
import Day4.PaperArea 

readPaperArea :: String -> PaperArea
readPaperArea = areaInicial . lines

contarPaperRollDisponibles :: PaperArea -> Int
contarPaperRollDisponibles pa
    | puedeMover Sur  pa = if puedeMover Este pa
                           then let pa' = mover Este pa
                                in fromEnum (hayPaperRollDisponible pa) + contarPaperRollDisponibles pa'
                           else let pa' = mover Sur (volver Oeste pa)
                                in fromEnum (hayPaperRollDisponible pa) + contarPaperRollDisponibles pa'
    | puedeMover Este pa = let pa' = mover Este pa
                           in fromEnum (hayPaperRollDisponible pa) + contarPaperRollDisponibles pa'
    | otherwise = fromEnum (hayPaperRollDisponible pa)

contarPaperRollsBorrados :: PaperArea -> Int
contarPaperRollsBorrados pa
    | hayAlgoParaBorrar pa = let (n,pa') = borrarPaperRolls pa pa
                             in n + contarPaperRollsBorrados pa'
    | otherwise            = 0

borrarPaperRolls :: PaperArea -> PaperArea -> (Int, PaperArea)
borrarPaperRolls paA paV
    | puedeMover Sur paV = if puedeMover Este paV
                           then let (n, paA') = borrar paA
                                    paV'      = mover Este paV
                                    (nr, pa)  = borrarPaperRolls (mover Este paA') paV'
                                in (n+nr, pa)
                           else let (n, paA') = borrar paA
                                    paV'      = mover Sur (volver Oeste paV)
                                    (nr, pa)  = borrarPaperRolls (mover Sur (volver Oeste paA')) paV'
                                in (n+nr, pa)
    | puedeMover Este paV = let (n, paA') = borrar paA
                                paV'      = mover Este paV
                                (nr, pa)  = borrarPaperRolls (mover Este paA') paV'
                            in (n+nr, pa)
    | otherwise = let (n,paA') = borrar paA
                  in (n, volver Norte (volver Oeste paA'))
    where
        borrar :: PaperArea -> (Int, PaperArea)
        borrar pa = if hayPaperRollDisponible pa then (1, sacar pa) else (0, pa)

-- ## ---------------------------------- aux --------------------------------------- ## --
hayAlgoParaBorrar :: PaperArea -> Bool
hayAlgoParaBorrar pa
    | puedeMover Sur  pa = if puedeMover Este pa
                           then let pa' = mover Este pa
                                in hayPaperRollDisponible pa || hayAlgoParaBorrar pa'
                           else let pa' = mover Sur (volver Oeste pa)
                                in hayPaperRollDisponible pa || hayAlgoParaBorrar pa'
    | puedeMover Este pa = let pa' = mover Este pa
                           in hayPaperRollDisponible pa || hayAlgoParaBorrar pa'
    | otherwise = hayPaperRollDisponible pa

volver :: Dir -> PaperArea -> PaperArea
volver d pa = if puedeMover d pa
              then let pa' = mover d pa
                   in volver d pa'
              else pa

hayPaperRollDisponible :: PaperArea -> Bool
hayPaperRollDisponible pa = hayPaperRoll pa && cantRollsEn pa Norte Este
                                             + cantRollsEn pa Este  Sur
                                             + cantRollsEn pa Sur   Oeste
                                             + cantRollsEn pa Oeste Norte < 4

cantRollsEn :: PaperArea -> Dir -> Dir -> Int
-- Es una función "Virtual" eso significa que en realidad NO me muevo al norte y al este, entonces no tengo que volver!
cantRollsEn pa d1 d2 = if puedeMover d1 pa
                       then let pa' = mover d1 pa
                            in fromEnum (hayPaperRoll pa') + if puedeMover d2 pa'
                                                            then fromEnum (hayPaperRoll (mover d2 pa'))
                                                            else 0
                       else 0