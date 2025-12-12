module Day4.PaperRolls where
import Day4.PaperArea

    -- TAD PaperArea:
    -- * areaInicial :: [String] -> PaperArea -- PRECOND: Cada caracter debe ser '@' o '.'
    -- * mover :: Dir -> PaperArea -> PaperArea -- PRECOND: La dirección a mover debe ser pósible
    -- * puedeMover :: Dir -> PaperArea -> Bool
    -- * hayPaperRoll :: PaperArea -> Bool

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
    | hayAlgoParaBorrar pa = let (n,pa') = marcarRolls pa
                             in n + contarPaperRollsBorrados (sacarMarcados pa')
    | otherwise            = 0

-- ## ---------------------------------- aux --------------------------------------- ## --
marcarRolls :: PaperArea -> (Int, PaperArea)
marcarRolls pa
    | puedeMover Sur  pa = if puedeMover Este pa
                           then let (n,pa')  = f hayPaperRollDisponible pa
                                    (m,pa'') = marcarRolls (mover Este pa')
                                in (n+m,pa'')
                           else let (n,pa')  = f hayPaperRollDisponible pa
                                    (m,pa'') = marcarRolls (mover Sur (volver Oeste pa'))
                                in (n+m,pa'')
    | puedeMover Este pa = let (n,pa')  = f hayPaperRollDisponible pa
                               (m,pa'') = marcarRolls (mover Este pa')
                           in (n+m,pa'')
    | otherwise = let pa' = volver Oeste pa
                  in (0,volver Norte pa')
    where
        f :: (PaperArea -> Bool) -> PaperArea -> (Int, PaperArea)
        f p pa_ = if p pa_ then (1,marcar pa_) else (0,pa_)

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
                            in fromEnum(hayPaperRoll pa') + if puedeMover d2 pa'
                                                            then fromEnum(hayPaperRoll(mover d2 pa'))
                                                            else 0
                       else 0