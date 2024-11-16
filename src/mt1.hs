-- MT1 Typ Datei
-- Datum: 16.11.24

module MT1 where
import Element
import Menge


newtype MT1 = MT1 [Element]

instance Menge MT1 where
    leereMenge = MT1 []
    allMenge   = MT1 allElems

    istMenge (MT1     []) = True
    istMenge (MT1 (_:[])) = True
    istMenge (MT1 (e:es)) = all (/= e) es
                            && istMenge (MT1 es)

    vereinige m1@(MT1 elems1) m2@(MT1 elems2) =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else MT1 $ nub $ elems1 ++ elems2

    schneide m1@(MT1 elems1) m2@(MT1 elems2) =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else MT1 $ dup $ elems1 ++ elems2

    zieheab m1@(MT1 elems1) m2@(MT1 elems2) =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else MT1 $ [e | e <- elems1, e `notElem` elems2]

    komplementiere = zieheab allMenge

    sindGleich m1 m2 = istTeilmenge m1 m2
                       && istTeilmenge m2 m1

    istTeilmenge m1@(MT1 elems1) m2@(MT1 elems2) =
        if not (istMenge m1 && istMenge m2)
        then error "Argument muss Menge sein (keine Duplikate)"
        else all (`elem` elems2) elems1

    istObermenge m1 m2 = istTeilmenge m2 m1

    zeige (MT1 elems) = "{" ++ formatElems elems ++ "}"
