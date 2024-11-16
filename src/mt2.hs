-- MT2 Typ Datei
-- Datum: 16.11.24

module MT2 where
import Element
import Menge


data MT2 = Nichts
           | VerlaengereUm Element MT2

instance Menge MT2 where
