-- Element Typ Datei
-- Datum: 16.11.24

module Element where


type Element = Char

-- Alle moegliche Elemente des Alphabets als Liste.
allElems :: [Element]
allElems = ['a'..'z'] ++ ['A'..'Z']

-- Formatiere Elemente, um sie auszudrucken.
formatElems :: [Element] -> String
formatElems []     = ""
formatElems [e]    = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es
