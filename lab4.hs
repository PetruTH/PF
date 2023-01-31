
-- Functia myzip3 care se comporta ca functia zip, dar are 3 argumente, creeaza un tuplu de 3 elemente
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 l1 l2 l3 = [ (a,b,c) | (a,(b,c)) <- zip l1 $ zip l2 l3] -- cele doua zip uri ne dau elemente de forma (a, (b,c)) pe care le rescriem (a,b,c)


-- Functie care ea lista primelor elemente dintr-o lista de tupluri

firstEl :: [(a,b)] -> [a]
firstEl ls = map fst ls         -- fst scoate primul element dintr-un tuplu


-- Functie care primeste un caracter si o lista de siruri si returneaza lista de siruri care contine caracterul respectiv

siruri :: Char -> [String] -> [String]
siruri c ls = filter (elem c) ls    -- aplicam 'elem c' pe sirurile noastre si returneaza true daca caracterul se afla in el



-- Functie care primeste o lista de siruri de caractere si returneaza lista sirurilor in care au fost eliminate consoanele

numaiVocale :: [String] -> [String]
numaiVocale ls = map (filter (`elem` "aeiouAEIOU")) ls  -- pt fiecare sir din lista vom filtra numai vocalele, predicatul din filter fiind `elem` "aeiouAEIOU"


-- Definiti recursiv mymap si myfilter cu aceleasi functionalitati ca cele predefinite

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = f x : mymap f xs     -- aplicam functia f pe fiecare element al listei

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter pred (x : xs) = if pred x then x : myfilter pred xs   -- verificam daca predicatul returneaza true pentru numarul din lista, daca da il adaugam la rezultat
                            else myfilter pred xs

                            

