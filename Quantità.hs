-- | Quantità e limiti
module Cassetta where


data Richiesta q a = Minimo (Tags a) q | Massimo (Tags a) q deriving (Show,Read)

data Presenza q a = Offerta (Tags a) q | Acquisto (Tags a) q


prodottiSep = foldr f ([],[]) where
	f (Offerta p x)  = first ((p,x):)
	f (Acquisto p x) = second ((p,x):)

prodottiTot ps = let (xs,ys) = prodottiSep ps in xs ++ ys

newtype Cassetta q b a = Cassetta {
	richieste :: [Richiesta q a],
	prodotti :: [Presenza q b a]
	} deriving (Show,Read) 

qtags :: Num q => Tags a -> [Presenza q b a] -> q
qtags ts ps = sum . map snd . prodottiTot . 
