{-# LANGUAGE NoMonomorphismRestriction, GADTs, DataKinds, KindSignatures,  FlexibleInstances, StandaloneDeriving, ScopedTypeVariables #-}
module Algorithm where

import Data.List (zipWith3)
import Data.Array.ST
import Data.Array
import Data.Array.MArray
import Control.Monad.ST
import Control.Arrow (second, (***))
import Data.Ratio
import System.Random.Shuffle 
import Genetica2

-- | Un incontro prevede uno scambio di valori interno tra due valori dello stesso tipo
type Incontro a = a -> a -> (a,a)

-- | Incontro senza scambio
zero :: Incontro a
zero = (,)

-- | esegue l'incontro come se gli elementi fossero alternati
swap :: Incontro a -> Incontro a 
swap f x y = let (y',x') = f y x  in (x',y')

swap' f x y = f y x
split :: Incontro a -> [a] -> [a]
split f xs = concatMap (either return $ \(x,y) -> [x,y]) $ zipSpl xs where
	zipSpl []  = []
	zipSpl [x]  = [Left x]
	zipSpl (x:y:xs) = Right (f x y): zipSpl xs


data Interesse = Positivo | Neutro | Negativo deriving (Show,Eq)

data Conversione = Convergente | Converso
data Prodotto q :: Conversione -> *
	where 	Proposto :: Interesse -> !q -> Prodotto q Convergente
		Disposto :: q -> Prodotto q Converso
	
instance Show q => Show (Prodotto q Converso) where
	show (Disposto x) = show x
instance Eq q => Eq (Prodotto q Convergente) where
	(Proposto _ q) == (Proposto _ q') = q == q'

trasfer ::forall q . (Num q, Ord q) => q -> Incontro (Prodotto q Convergente) 
trasfer dqs x y = trasf x y where
	fake :: Interesse -> Incontro (Prodotto q Convergente)
	fake i q (Proposto j x) = second (\(Proposto i x) -> Proposto j x) $ trasf q (Proposto i x)

	trasf :: Incontro (Prodotto q Convergente)
	trasf	x@(Proposto Positivo q) y@(Proposto Negativo q') 
		| q' - dqs >= 0 = (Proposto Positivo (q + dqs), Proposto Negativo (q' - dqs))
		| otherwise = (x,y)
	trasf x@(Proposto Negativo _) 	y@(Proposto Positivo _) 	= swap trasf x y
	trasf x@(Proposto Positivo _) 	y@(Proposto Neutro _) 		= fake Negativo x y 
	trasf x@(Proposto Neutro _) 	y@(Proposto Positivo _)		= swap trasf x y
	trasf x@(Proposto Negativo _) 	y@(Proposto Neutro _) 		= fake Positivo x y 
	trasf x@(Proposto Neutro _) 	y@(Proposto Negativo _) 	= swap trasf x y
	trasf x@(Proposto Neutro _) 	y@(Proposto Neutro _) 		= (x,y)
	trasf x@(Proposto Negativo _) 	y@(Proposto Negativo _)		= fake Positivo  x y 
	trasf x@(Proposto Positivo _) 	y@(Proposto Positivo _) 	= fake Negativo  x y 

type Rank = Int


data Carrello l  = Carrello Rank [Prodotto Rational l] 

disposto :: Carrello Convergente  -> Carrello Converso 
disposto (Carrello r xs) = Carrello r $ map (\(Proposto i q) -> Disposto q) xs

instance Show (Carrello Converso) where
	show (Carrello i  xs) = show (i, map f xs)
		where f (Disposto q) = Disposto (fromRational q) 

deriving instance Eq (Carrello Convergente)

type Quanti = [Rational]

correct :: Quanti -> Incontro (Carrello Convergente)
correct qs cx@(Carrello rx xs) cy@(Carrello ry ys) 
	| rx < ry = (Carrello rx *** Carrello ry) . unzip $ zipWith3 trasfer qs xs ys
	| otherwise = swap (correct qs) cx cy


splitM f xs = split f `fmap` shuffleM xs

	
v = repeat 0.1 
	
p = Carrello 0 [Proposto Positivo 1, Proposto Positivo 2, Proposto Neutro 2]
q = Carrello 1 [Proposto Positivo 1, Proposto Negativo 2, Proposto Neutro 2]
r = Carrello 2 [Proposto Positivo 0, Proposto Neutro 1, Proposto Neutro 3 ]

