{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, DataKinds, GADTs, BangPatterns, NoMonomorphismRestriction #-}
import Prelude hiding (foldr, sum, concatMap)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Array (listArray, array, Array, elems, bounds, (!))
import Data.Ix (Ix)
import Data.Ratio (Rational)
import Control.Arrow ((&&&), second)
import System.Random (StdGen, split)
import System.Random.Shuffle (shuffle')
import Data.Traversable (mapAccumL)
import Control.Monad (ap)
import Data.Foldable (foldr, sum, concatMap)

import Genetica2 

newtype Utente = Utente Int deriving (Ord,Ix,Eq, Num, Enum)
newtype Prodotto = Prodotto Int deriving (Ord,Ix,Eq, Num)

newtype Quantità = Quantità Rational deriving (Num, Ord, Eq)


data Interesse = Positivo | Neutro | Negativo


-- | esegue l'incontro come se gli elementi fossero alternati
swap f x y = let (y',x') = f y x  in (x',y')

trasfer :: Quantità -> (Interesse, Quantità) -> (Interesse, Quantità) -> (Quantità, Quantità)
trasfer dqs = trasf where
	fake i q (j, x) =  trasf q (i, x)

	trasf	x@(Positivo, q) y@(Negativo, q') 
		| q' - dqs >= 0 = ((q + dqs), (q' - dqs))
		| otherwise = (q,q')
	trasf x@(Negativo, _) 	y@(Positivo, _) = swap trasf x y
	trasf x@(Positivo, _) 	y@(Neutro, _) 	= fake Negativo x y 
	trasf x@(Neutro, _) 	y@(Positivo, _)	= swap trasf x y
	trasf x@(Negativo, _) 	y@(Neutro, _) 	= fake Positivo x y 
	trasf x@(Neutro, _) 	y@(Negativo, _) = swap trasf x y
	trasf x@(Neutro, q) 	y@(Neutro, q') 	= (q,q')
	trasf x@(Negativo, _) 	y@(Negativo, _)	= fake Positivo  x y 
	trasf x@(Positivo, _) 	y@(Positivo, _) = fake Negativo  x y 

newtype Rank = Rank Int deriving (Ord, Eq)

newtype Matrice q = Matrice (DatiUtente (DatiProdotto q))

zipMatrice f = zipArray (zipArray f)

zipArray :: Ix i => (a -> b -> c) -> Array i a -> Array i b -> Array i c
zipArray f a = snd . mapAccumL (\(x:xs) y -> (xs, f x y)) (elems a)

type DatiUtente q = Array Utente q
type DatiProdotto q = Array Prodotto q

type Ranks  = DatiUtente Rank
type Quanti = DatiProdotto Quantità

newtype Rapporto = Rapporto Rational deriving (Ord, Eq, Num)

type instance Valore (Matrice Quantità) = Rapporto

newGen :: StdGen -> StdGen
newGen = fst . split

flex :: [Rapporto] -> Bool
flex [] = False
flex [_] = False
flex [_,_] = False
flex (vm:vx:vM:_) = vx >= vm && vx >= vM

data Giudizio = Giudizio [Prodotto] (Quantità -> Bool)

valutazione :: DatiUtente [Giudizio] ->  Matrice Quantità -> Rapporto
valutazione vg (Matrice mq) = foldr f 0 (zipArray g vg mq) where
	g :: [Giudizio] -> DatiProdotto Quantità -> [Bool]
	g gs aq = map (\(Giudizio ps fq) -> fq . sum . map (aq !) $ ps) gs
	f :: [Bool] -> Rapporto -> Rapporto 
	f bs (Rapporto r) = let (rs,ws) = foldr (\x (rs,ws) -> if x then (rs + 1, ws) else (rs,ws + 1)) (1,1) bs 
		in Rapporto $ r + rs / ws

 
newtype Limite = Limite Int deriving (Num,Ord,Eq)

newtype Giudizi = Giudizi (DatiUtente [Giudizio])

operatore 	::   	Limite 	-- ^ numero massimo di iterazioni a caccia del massimo
		-> 	Quanti  -- ^ quantità unitarie di trasferimento
		-> 	Ranks 	-- ^ classifica di biasimo
		-> 	Giudizi -- ^ preferenze utenti
		-> 	Matrice Interesse -- ^ descrizione operatore
		-> 	StdGen 	-- ^ seme random iniziale
		-> 	Elemento (Variazioni Soluzioni) (Matrice Quantità) -- ^ operatore di primo livello
operatore l qs rs (Giudizi gs) (Matrice mi) = Operatore . f  
	where	(1,u) = bounds mi
		(1,p) = bounds qs
		f s (Soluzione mq) = (Operatore $ f (newGen s), second soluzione nmq) 
			where	vmq = valutazione gs mq
				nmq = corri s (Limite 0) [vmq] $ mq 
				corri :: StdGen -> Limite -> [Rapporto] -> Matrice Quantità -> (Rapporto,Matrice Quantità)
				corri s !n pvs mq@(Matrice aq) = if	flex nvs || n >= l 
							then 	(nvs !! 1, mq) 
							else  	corri (newGen s) (n + 1) nvs nmq 
					where	ijs 	= zip `ap` drop (fromEnum u `div` 2) $ shuffle' [1..u] (fromEnum u) s
						nmq 	= Matrice $ array (1,u) $ concatMap coppie ijs 
						nvs 	= valutazione gs nmq : pvs
						miq 	= zipArray (zipArray (,)) mi aq
						coppie (i,j) = [(i',listArray (1,p) mi'), (j',listArray (bounds qs) mi'')] 
							where	(i',j') 	= if rs ! i <= rs ! j 
											then (i,j) 
											else (j,i)
								(mi',mi'') 	= unzip $ zipWith3 trasfer (elems qs) (elems $ miq ! i') (elems $ miq ! j')

soluzione :: Matrice Quantità -> Elemento Soluzioni (Matrice Quantità)
soluzione = Soluzione

tagliaSoluzioni n xs = take n $ sortBy (comparing (negate . fst)) $ elems xs
tagliaOperatori = tagliaSoluzioni

