-- MT2 Typ Datei
-- Datum: 16.11.24

module MT2 where
import Element
import Menge


data MT2 = Nichts
           | VerlaengereUm Element MT2

instance Menge MT2 where
    leereMenge = Nichts
    allMenge   = fromElems allElems

    istMenge Nichts = True
    istMenge (VerlaengereUm e m) =
        (not . isElem e) m && istMenge m

    vereinige Nichts Nichts = Nichts
    vereinige m Nichts =
        if not $ istMenge m
        then error "Argument muss Menge sein (keine Duplikate)"
        else m
    vereinige Nichts m = vereinige m Nichts
    vereinige m1 m2@(VerlaengereUm e m2') =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else if (not . isElem e) m1
             then vereinige (VerlaengereUm e m1) m2'
             else vereinige m1 m2'

    schneide Nichts Nichts = Nichts
    schneide Nichts m =
        if not $ istMenge m
        then error "Argument muss Menge sein (keine Duplikate)"
        else Nichts
    schneide m Nichts = schneide Nichts m
    schneide m1 m2 =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else intersectMt2 Nichts m1 m2

    zieheab Nichts Nichts = Nichts
    zieheab Nichts m =
        if not $ istMenge m
        then error "Argument muss Menge sein (keine Duplikate)"
        else Nichts
    zieheab m Nichts =
        if not $ istMenge m
        then error "Argument muss Menge sein (keine Duplikate)"
        else m
    zieheab m1 m2@(VerlaengereUm e m2') =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else if isElem e m1
             then zieheab (removeElem e m1) m2'
             else zieheab m1 m2'

    komplementiere = zieheab allMenge

    sindGleich m1 m2 = istTeilmenge m1 m2
                       && istTeilmenge m2 m1

    istTeilmenge Nichts Nichts = True
    istTeilmenge Nichts m =
        if not $ istMenge m
        then error "Argument muss Menge sein (keine Duplikate)"
        else True
    istTeilmenge m Nichts =
        if not $ istMenge m
        then error "Argument muss Menge sein (keine Duplikate)"
        else False
    istTeilmenge m1@(VerlaengereUm e m1') m2 =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else if isElem e m2
             then istTeilmenge m1' m2
             else False

    istObermenge m1 m2 = istTeilmenge m2 m1

    zeige m = "{" ++ (formatElems . mt2Elems) m ++ "}"


-- Ob ein Element ist Element von einem MT2.
isElem :: Element -> MT2 -> Bool
isElem _  Nichts = False
isElem e1 (VerlaengereUm e2 m) =
    if e1 == e2 then True else isElem e1 m

-- Wandle eine Liste von Elementen in einem MT2 um.
fromElems :: [Element] -> MT2
fromElems []     = Nichts
fromElems (e:es) = VerlaengereUm e $ fromElems es

-- Wandle ein MT2 in einer Liste von Elementen um.
mt2Elems :: MT2 -> [Element] 
mt2Elems Nichts              = []
mt2Elems (VerlaengereUm e m) = e : (mt2Elems m)

-- Schneide zwei MT2 zusammen mit einem Akumulator.
-- Man sollte den Akumulator mit `Nichts` starten.
intersectMt2 :: MT2 -> MT2 -> MT2 -> MT2
intersectMt2 acc Nichts _ = acc
intersectMt2 acc _ Nichts = acc
intersectMt2 acc m1 (VerlaengereUm e m') =
    if isElem e m1
    then intersectMt2 (VerlaengereUm e acc) m1 m'
    else intersectMt2 acc m1 m'

-- Entferne ein Element von einem MT2.
removeElem :: Element -> MT2 -> MT2
removeElem _ Nichts = Nichts
removeElem e' (VerlaengereUm e m') =
    if e == e' then m' else VerlaengereUm e $ removeElem e' m'
