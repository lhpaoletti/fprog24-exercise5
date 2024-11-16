-- MT1 Typ Datei
-- Datum: 16.11.24

module MT1 where
import Element
import Menge


newtype MT1 = MT1 [Element]

instance Menge MT1 where
