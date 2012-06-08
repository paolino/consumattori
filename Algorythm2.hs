{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DataKinds, GADTs, BangPatterns #-}
import Data.Array
import Data.Ix
import Data.Ratio
import Control.Arrow
import System.Random
import System.Random.Shuffle
import Data.Traversable
import Control.Monad
import Data.Foldable hiding (concatMap)

import Genetica2 

newtype Utente = Utente Int deriving (Ord,Ix,Eq, Num, Enum)
newtype Prodotto = Prodotto Int deriving (Ord,Ix,Eq)

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

mix :: [Quantità] -> [(Interesse,Quantità)] -> [(Interesse,Quantità)] -> [(Quantità,Quantità)]
mix = zipWith3 trasfer

newtype Rank = Rank Int deriving (Ord, Eq)

newtype Matrice q = Matrice (Array Utente [q])
type Ranks  = Array Utente Rank
type Quanti = [Quantità]

newtype Bounds = Bounds (Utente,Prodotto)

newtype Rapporto = Rapporto Rational deriving (Ord, Eq, Num)

type instance Valore (Matrice Quantità) = Rapporto

valutazione :: Matrice Quantità -> Rapporto
valutazione = undefined

flex :: [Rapporto] -> Bool
flex [] = False
flex [_] = False
flex [_,_] = False
flex (vm:vx:vM:_) = vx >= vm && vx >= vM


opera :: Bounds -> StdGen -> Quanti -> Ranks -> Matrice Interesse -> Matrice Quantità -> Matrice Quantità
opera b@(Bounds (u,_)) s qs rs mi mq = let 
	ijs = zip `ap` drop (fromEnum u `div` 2) $ shuffle' [1..u] (fromEnum u) s
	in svolgiOperazione  b  rs qs (zipMatrice mi mq) ijs
	{-
	zippa matrici
	prendi coppie di colonne
-}

zipMatrice :: Matrice a -> Matrice b -> Matrice (a,b)
zipMatrice (Matrice m1) (Matrice m2) = Matrice . snd $ mapAccumL (\(x:xs) y -> (xs,zip x y)) (toList m1) m2


svolgiOperazione :: Bounds -> Ranks -> Quanti -> Matrice (Interesse,Quantità) -> [(Utente,Utente)] -> Matrice Quantità
svolgiOperazione (Bounds (u,_)) rs qs (Matrice m) ijs = Matrice $ array (1,u) $ concatMap coppie ijs
	where coppie (i,j) = let
		(i',j') = if rs ! i <= rs ! j then (i,j) else (j,i)
		(mi',mi'') = unzip $ mix qs (m ! i') (m ! j')
		in [(i',mi'), (j',mi'')]




newtype Limite = Limite Int deriving (Num,Ord,Eq)

modifica :: Bounds -> Limite -> Quanti -> Ranks -> Matrice Interesse -> StdGen -> Matrice Quantità -> (Rapporto, Matrice Quantità)
modifica b@(Bounds (u,_)) l qs rs mi s mq =  modifica' s (Limite 0) [valutazione mq] mq where
	modifica' :: StdGen -> Limite -> [Rapporto] -> Matrice Quantità -> (Rapporto, Matrice Quantità)
	modifica' s !n pvs  mq@(Matrice aq) = let
		(s',_) = split s 
		mq' = opera b s qs rs mi mq
		nvs = (:pvs) $ valutazione mq'
		in case flex nvs || n >= l of
			True -> (head nvs, mq)
			False -> modifica' s' (n + 1) nvs mq'


operatore ::  Bounds -> Limite -> Quanti -> Ranks -> Matrice Interesse -> StdGen -> Elemento (Variazioni Soluzioni) (Matrice Quantità) 
operatore b l q r mi = Operatore . f  where
	f s (Soluzione mq) = (operatore b l q r mi (fst . split $ s), second Soluzione $ modifica b l q r mi s mq) 
		
soluzione :: Matrice Quantità -> Elemento Soluzioni (Matrice Quantità)
soluzione = Soluzione

	{-
	is = shuffle s $ indices rs
	(js,ks) = splitAt (size rs `div` 2)
	miq = x
	-}

