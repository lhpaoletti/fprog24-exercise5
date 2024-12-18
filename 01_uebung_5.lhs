> type Element = Char

Alle moegliche Elemente des Alphabets als Liste.

> allElems :: [Element]
> allElems = ['a'..'z'] ++ ['A'..'Z']

Entferne Duplikate einer Liste.

> nub :: [Element] -> [Element]
> nub []     = []
> nub (e:es) = e : (nub $ filter (/= e) es)

Lasse nur Duplikate einer Liste bleiben.

> dup :: [Element] -> [Element]
> dup [] = []
> dup (e:es) =
>     if e `elem` es
>     then e : (dup es)
>     else dup es

Formatiere Elemente, um sie auszudrucken.

> formatElems :: [Element] -> String
> formatElems []     = ""
> formatElems [e]    = show e
> formatElems (e:es) = show e ++ ", " ++ formatElems es



> class Menge a where
>     leereMenge :: a
>     allMenge   :: a
>     istMenge :: a -> Bool
>     vereinige :: a -> a -> a
>     schneide  :: a -> a -> a
>     zieheab   :: a -> a -> a
>     komplementiere :: a -> a
>     sindGleich   :: a -> a -> Bool
>     istTeilmenge :: a -> a -> Bool
>     istObermenge :: a -> a -> Bool
>     zeige :: a -> String
>
>     istMenge = \_ -> True



--- MT1 ---

> newtype MT1 = MT1 [Element]

> instance Menge MT1 where
>     leereMenge = MT1 []
>     allMenge   = MT1 allElems
>
>     istMenge (MT1     []) = True
>     istMenge (MT1 (_:[])) = True
>     istMenge (MT1 (e:es)) = all (/= e) es
>                             && istMenge (MT1 es)
>
>     vereinige m1@(MT1 elems1) m2@(MT1 elems2) =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         -- Fuege beide Mengen zusammen, dann nimm Duplikate weg
>         else MT1 $ nub $ elems1 ++ elems2
>
>     schneide m1@(MT1 elems1) m2@(MT1 elems2) =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         -- Fuege beide Mengen zusammen, dann lass nur Duplikate drin
>         else MT1 $ dup $ elems1 ++ elems2
>
>     zieheab m1@(MT1 elems1) m2@(MT1 elems2) =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         -- Nimm nur Elemente der ersten Menge, die nicht in der Zweiten vorkommen
>         else MT1 $ [e | e <- elems1, e `notElem` elems2]
>
>     komplementiere = zieheab allMenge
>
>     sindGleich m1 m2 = istTeilmenge m1 m2
>                        && istTeilmenge m2 m1
>
>     istTeilmenge m1@(MT1 elems1) m2@(MT1 elems2) =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else all (`elem` elems2) elems1
>
>     istObermenge m1 m2 = istTeilmenge m2 m1
>
>     zeige (MT1 elems) = "{" ++ formatElems elems ++ "}"



--- MT2 ---

> data MT2 = Nichts
>            | VerlaengereUm Element MT2

> instance Menge MT2 where
>     leereMenge = Nichts
>     allMenge   = fromElems allElems
>
>     istMenge Nichts = True
>     istMenge (VerlaengereUm e m) =
>         (not . isElem e) m && istMenge m
>
>     vereinige Nichts Nichts = Nichts
>     vereinige Nichts m = vereinige m Nichts
>     vereinige m Nichts =
>         if not $ istMenge m
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else m
>     vereinige m1 m2@(VerlaengereUm e m2') =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else if not . isElem e $ m1
>              -- Akumuliere neue einzigartige Elemente der zweiten Menge in der Ersten
>              then vereinige (VerlaengereUm e m1) m2'
>              else vereinige m1 m2'
>
>     schneide Nichts Nichts = Nichts
>     schneide m Nichts = schneide Nichts m
>     schneide Nichts m =
>         if not $ istMenge m
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else Nichts
>     schneide m1 m2 =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else intersectMt2 Nichts m1 m2
>
>     zieheab Nichts Nichts = Nichts
>     zieheab Nichts m =
>         if not $ istMenge m
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else Nichts
>     zieheab m Nichts =
>         if not $ istMenge m
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else m
>     zieheab m1 m2@(VerlaengereUm e m2') =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else if isElem e m1
>              -- Nimm von der ersten Menge Elemente weg, die bei der Zweiten auch sind
>              then zieheab (removeElem e m1) m2'
>              else zieheab m1 m2'
>
>     komplementiere = zieheab allMenge
>
>     sindGleich m1 m2 = istTeilmenge m1 m2
>                        && istTeilmenge m2 m1
>
>     istTeilmenge Nichts Nichts = True
>     istTeilmenge Nichts m =
>         if not $ istMenge m
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else True
>     istTeilmenge m Nichts =
>         if not $ istMenge m
>         then error "Argument muss Menge sein (keine Duplikate)"
>         else False
>     istTeilmenge m1@(VerlaengereUm e m1') m2 =
>         if not (istMenge m1 && istMenge m2)
>         then error "Argument muss Menge sein (keine Duplikate)"
>         -- Die erste Menge ist Teilmenge, wenn jedes ihrer Elemente in der Zweiten auch ist
>         else if isElem e m2
>              then istTeilmenge m1' m2
>              else False
>
>     istObermenge m1 m2 = istTeilmenge m2 m1
>
>     zeige m = "{" ++ (formatElems . mt2Elems) m ++ "}"


Ob ein Element ist Element von einem MT2.

> isElem :: Element -> MT2 -> Bool
> isElem _  Nichts = False
> isElem e1 (VerlaengereUm e2 m) =
>     if e1 == e2 then True else isElem e1 m

Wandle eine Liste von Elementen in einem MT2 um.

> fromElems :: [Element] -> MT2
> fromElems []     = Nichts
> fromElems (e:es) = VerlaengereUm e $ fromElems es

Wandle ein MT2 in einer Liste von Elementen um.

> mt2Elems :: MT2 -> [Element] 
> mt2Elems Nichts              = []
> mt2Elems (VerlaengereUm e m) = e : (mt2Elems m)

Schneide zwei MT2 zusammen mit einem Akumulator.
Man sollte den Akumulator mit `Nichts` starten.

> intersectMt2 :: MT2 -> MT2 -> MT2 -> MT2
> intersectMt2 acc Nichts _ = acc
> intersectMt2 acc _ Nichts = acc
> intersectMt2 acc m1 (VerlaengereUm e m') =
>     -- Elemente der zweiten Menge werden akumuliert, wenn sie auch in der Ersten sind
>     if isElem e m1
>     then intersectMt2 (VerlaengereUm e acc) m1 m'
>     else intersectMt2 acc m1 m'

Entferne ein Element von einem MT2.

> removeElem :: Element -> MT2 -> MT2
> removeElem _ Nichts = Nichts
> removeElem e' (VerlaengereUm e m') =
>     if e == e' then m' else VerlaengereUm e $ removeElem e' m'



--- MT3 ---

> newtype MT3 = MT3 (Element -> Bool)

istMenge muss nicht ueberschrieben werden, weil es nicht
 moeglich ist, dass ein MT3 nicht eine Menge sei

> instance Menge MT3 where
>     leereMenge = MT3 (\_ -> False)
>     allMenge   = MT3 (\_ -> True )
>
>     vereinige (MT3 f1) (MT3 f2) = MT3 $
>         \elem -> f1 elem || f2 elem
>
>     schneide (MT3 f1) (MT3 f2) = MT3 $
>         \elem -> f1 elem && f2 elem
>
>     zieheab (MT3 f1) (MT3 f2) = MT3 $
>         \elem -> f1 elem && (not . f2) elem
>
>     komplementiere (MT3 f) = MT3 (not . f)
>
>     sindGleich m1 m2@(MT3 f) =
>         let elems1 = mt3Elems m1
>             elems2 = mt3Elems m2
>         in istTeilmenge m1 m2
>            && istTeilmenge m2 m1
>
>     istTeilmenge m1 (MT3 f) =
>         let elems1 = mt3Elems m1
>         in all f elems1
>
>     istObermenge m1 m2 = istTeilmenge m2 m1
>
>     zeige m = "{" ++ (formatElems . mt3Elems) m ++ "}"


Hole die Elemente von einem MT3 als Liste.

> mt3Elems :: MT3 -> [Element]
> mt3Elems (MT3 f) = [e | e <- allElems, f e]



--- main ---

> main = do
>     putStrLn ""
>     putStrLn $ "---------------------------MT1----------------------------"
>     putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1)
>     putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1)
>     putStrLn ""
>     putStrLn $ "istMenge         {}: " ++ (show $ istMenge (leereMenge :: MT1))
>     putStrLn $ "istMenge {'a', 'a'}: " ++ (show $ istMenge $ MT1 "aa")
>     putStrLn $ "istMenge {'a', 'b'}: " ++ (show $ istMenge $ MT1 "ab")
>     putStrLn ""
>     putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ MT1 "a")
>     putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige (MT1 "a") (MT1 "a"))
>     putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige (MT1 "a") (MT1 "b"))
>     putStrLn ""
>     putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ MT1 "a")
>     putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide (MT1 "a") (MT1 "a"))
>     putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide (MT1 "a") (MT1 "ab"))
>     putStrLn ""
>     putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ MT1 "a")
>     putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab (MT1 "a") (MT1 "a"))
>     putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab (MT1 "ab") (MT1 "a"))
>     putStrLn ""
>     putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 "a")
>     putStrLn ""
>     putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich (MT1  "a") (MT1  "a"))
>     putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich (MT1  "a") (MT1  "b"))
>     putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich (MT1 "ab") (MT1 "ba"))
>     putStrLn ""
>     putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge (MT1  "a") (MT1  "a"))
>     putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge (MT1 "ab") (MT1 "ba"))
>     putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge (MT1  "a") (MT1 "ab"))
>     putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge (MT1 "ab") (MT1  "a"))
>     putStrLn ""
>     putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge (MT1  "a") (MT1  "a"))
>     putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge (MT1 "ab") (MT1 "ba"))
>     putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge (MT1  "a") (MT1 "ab"))
>     putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge (MT1 "ab") (MT1  "a"))
>     putStrLn ""
>     putStrLn ""
>     putStrLn $ "---------------------------MT2----------------------------"
>     let a2  = VerlaengereUm 'a' Nichts
>         b2  = VerlaengereUm 'b' Nichts
>         aa2 = VerlaengereUm 'a' $ VerlaengereUm 'a' Nichts
>         ab2 = VerlaengereUm 'a' $ VerlaengereUm 'b' Nichts
>         ba2 = VerlaengereUm 'b' $ VerlaengereUm 'a' Nichts
>
>     putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT2)
>     putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT2)
>     putStrLn ""
>     putStrLn $ "istMenge         {}: " ++ (show $ istMenge (leereMenge :: MT2))
>     putStrLn $ "istMenge {'a', 'a'}: " ++ (show $ istMenge aa2)
>     putStrLn $ "istMenge {'a', 'b'}: " ++ (show $ istMenge ab2)
>     putStrLn ""
>     putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ a2)
>     putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige a2 a2)
>     putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige a2 b2)
>     putStrLn ""
>     putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ a2)
>     putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide a2  a2)
>     putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide a2 ab2)
>     putStrLn ""
>     putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ a2)
>     putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab  a2 a2)
>     putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab ab2 a2)
>     putStrLn ""
>     putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ a2)
>     putStrLn ""
>     putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich  a2  a2)
>     putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich  a2  b2)
>     putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich ab2 ba2)
>     putStrLn ""
>     putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge  a2  a2)
>     putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge ab2 ba2)
>     putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge  a2 ab2)
>     putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge ab2  a2)
>     putStrLn ""
>     putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge  a2  a2)
>     putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge ab2 ba2)
>     putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge  a2 ab2)
>     putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge ab2  a2)
>     putStrLn ""
>     putStrLn ""
>     putStrLn $ "---------------------------MT3----------------------------"
>     let a3  = \e -> if e == 'a'             then True else False
>         b3  = \e -> if e == 'b'             then True else False
>         ab3 = \e -> if e == 'a' || e == 'b' then True else False
>         ba3 = \e -> if e == 'b' || e == 'a' then True else False
>
>     putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT3)
>     putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT3)
>     putStrLn ""
>     putStrLn "istMenge ist die Protoimplementierung"
>     putStrLn ""
>     putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ MT3 a3)
>     putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige (MT3 a3) (MT3 a3))
>     putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige (MT3 a3) (MT3 b3))
>     putStrLn ""
>     putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ MT3 a3)
>     putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide (MT3 a3) (MT3  a3))
>     putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide (MT3 a3) (MT3 ab3))
>     putStrLn ""
>     putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ MT3 a3)
>     putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab (MT3  a3) (MT3 a3))
>     putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab (MT3 ab3) (MT3 a3))
>     putStrLn ""
>     putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ MT3 a3)
>     putStrLn ""
>     putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich (MT3  a3) (MT3  a3))
>     putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich (MT3  a3) (MT3  b3))
>     putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich (MT3 ab3) (MT3 ba3))
>     putStrLn ""
>     putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge (MT3  a3) (MT3  a3))
>     putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge (MT3 ab3) (MT3 ba3))
>     putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge (MT3  a3) (MT3 ab3))
>     putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge (MT3 ab3) (MT3  a3))
>     putStrLn ""
>     putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge (MT3  a3) (MT3  a3))
>     putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge (MT3 ab3) (MT3 ba3))
>     putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge (MT3  a3) (MT3 ab3))
>     putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge (MT3 ab3) (MT3  a3))
