-- Element Typ Datei
-- Datum: 16.11.24

module Element where


type Element = Char

-- Alle moegliche Elemente des Alphabets als Liste.
allElems :: [Element]
allElems = ['a'..'z'] ++ ['A'..'Z']

-- Entferne Duplikate einer Liste.
nub :: [Element] -> [Element]
nub []     = []
nub (e:es) = e : (nub $ filter (/= e) es)

-- Lasse nur Duplikate einer Liste bleiben.
dup :: [Element] -> [Element]
dup [] = []
dup (e:es) =
    if e `elem` es
    then e : (dup es)
    else dup es

-- Formatiere Elemente, um sie auszudrucken.
formatElems :: [Element] -> String
formatElems []     = ""
formatElems [e]    = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es
