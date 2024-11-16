-- MT3 Typ Datei
-- Datum: 16.11.24

module MT3 where
import Element
import Menge


newtype MT3 = MT3 (Element -> Bool)

instance Menge MT3 where
