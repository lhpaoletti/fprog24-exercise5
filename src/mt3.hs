-- MT3 Typ Datei
-- Datum: 16.11.24

module MT3 where
import Element
import Menge


newtype MT3 = MT3 (Element -> Bool)

instance Menge MT3 where
    leereMenge = MT3 (\_ -> False)
    allMenge   = MT3 (\_ -> True )

    -- istMenge muss nicht ueberschrieben werden, weil es nicht
    --  moeglich ist, dass ein MT3 nicht eine Menge ist

    vereinige (MT3 f1) (MT3 f2) = MT3 $
        \elem -> f1 elem || f2 elem

    schneide (MT3 f1) (MT3 f2) = MT3 $
        \elem -> f1 elem && f2 elem

    zieheab (MT3 f1) (MT3 f2) = MT3 $
        \elem -> f1 elem && (not . f2) elem

    komplementiere (MT3 f) = MT3 (not . f)

    sindGleich m1 m2@(MT3 f) =
        let elems1 = mt3Elems m1
            elems2 = mt3Elems m2
        -- beide gleich wenn beide Teilmengen voneinander
        in istTeilmenge m1 m2
           && istTeilmenge m2 m1

    istTeilmenge m1 (MT3 f) =
        let elems1 = mt3Elems m1
        in all f elems1

    istObermenge m1 m2 = istTeilmenge m2 m1

    zeige m = "{" ++ (formatElems . mt3Elems) m ++ "}"


-- Hole die Elemente von einem MT3 als Liste.
mt3Elems :: MT3 -> [Element]
mt3Elems (MT3 f) = [e | e <- allElems, f e]
