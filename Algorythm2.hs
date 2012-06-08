{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DataKinds, GADTs #-}
import Data.Array
import Data.Ix
import Data.Ratio
import Control.Arrow
import System.Random
import System.Random.Shuffle

import Genetica2 

newtype Utente = Utente Int deriving (Ord,Ix,Eq, Num)
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
flex ((vm,_):(vx,_):(vM,_):_) = vx >= vm && vx >= vM


opera :: StdGen -> Ranks -> Matrice Interesse -> Matrice Quantità -> Matrice Quantità
opera = undefined

modifica :: Bounds -> Quanti -> Ranks -> Matrice Interesse -> StdGen -> Matrice Quantità -> (Rapporto, Matrice Quantità)
modifica b@(Bounds (u,p)) qs rs mi s mq =  modifica' s [valutazione mq] mq where
	modifica' :: StdGen -> [Rapporto] -> Matrice Quantità -> (Rapporto, Matrice Quantità)
	modifica' s pvs  mq@(Matrice aq) = let
		(s',_) = split s 
		mq' = opera s rs mi mq
		nvs = (:pvs) $ valutazione mq'
		in case flex nvs of
			True -> (head nvs, mq)
			False -> modifica' s' nvs mq'


operatore ::  Bounds -> Quanti -> Ranks -> Matrice Interesse -> StdGen -> Elemento (Variazioni Soluzioni) (Matrice Quantità) 
operatore b q r mi = Operatore . f  where
	f s (Soluzione mq) = (operatore b q r mi (fst . split $ s), second Soluzione $ modifica b q r mi s mq) 
		
soluzione :: Matrice Quantità -> Elemento Soluzioni (Matrice Quantità)
soluzione = Soluzione

	{-
	is = shuffle s $ indices rs
	(js,ks) = splitAt (size rs `div` 2)
	miq = x
	-}

