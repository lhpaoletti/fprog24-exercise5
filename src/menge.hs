-- Menge Typklasse Datei
-- Datum: 16.11.24

module Menge where


class Menge a where
    leereMenge :: a
    allMenge   :: a
    istMenge :: a -> Bool
    vereinige :: a -> a -> a
    schneide  :: a -> a -> a
    zieheab   :: a -> a -> a
    komplementiere :: a -> a
    sindGleich   :: a -> a -> Bool
    istTeilmenge :: a -> a -> Bool
    istObermenge :: a -> a -> Bool
    zeige :: a -> String

    istMenge = \_ -> True
