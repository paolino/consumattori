{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List (zipWith3)
import Control.Arrow (second, (***))

import System.Random.Shuffle (shuffleM)
import Control.Monad.Random.Class (MonadRandom)

swap :: (a -> b -> (c, d)) -> b -> a -> (d, c)
swap f x y = let (y',x') = f y x  in (x',y')

data Interesse = Positivo | Neutro | Negativo
data Prodotto q = Prodotto Interesse q 

type Twin a = a -> a -> (a,a)

trasfer :: (Num q, Ord q) => q -> Twin (Prodotto q) 
trasfer dqs x y = trasf x y where
	fake i q (Prodotto j x) = second (\(Prodotto i x) -> Prodotto j x) $ trasf q (Prodotto i x)
	trasf	x@(Prodotto Positivo q) y@(Prodotto Negativo q') 
		| q' - dqs >= 0 = (Prodotto Positivo (q + dqs), Prodotto Negativo (q' - dqs))
		| otherwise = (x,y)
	trasf x@(Prodotto Negativo _) 	y@(Prodotto Positivo _) 	= swap trasf x y
	trasf x@(Prodotto Positivo _) 	y@(Prodotto Neutro _) 		= fake Negativo x y 
	trasf x@(Prodotto Neutro _) 	y@(Prodotto Positivo _)		= swap trasf x y
	trasf x@(Prodotto Negativo _) 	y@(Prodotto Neutro _) 		= fake Positivo x y 
	trasf x@(Prodotto Neutro _) 	y@(Prodotto Negativo _) 	= swap trasf x y
	trasf x@(Prodotto Neutro _) 	y@(Prodotto Neutro _) 		= (x,y)
	trasf x@(Prodotto Negativo _) 	y@(Prodotto Negativo _)		= fake Positivo  x y 
	trasf x@(Prodotto Positivo _) 	y@(Prodotto Positivo _) 	= fake Negativo  x y 

type Rank = Int

data Carrello q = Carrello Rank [Prodotto q]

type Quanti q = [q]

correct :: (Num q, Ord q) => Quanti q -> Twin (Carrello q)
correct qs cx@(Carrello rx xs) cy@(Carrello ry ys) 
	| rx < ry = (Carrello rx *** Carrello ry) . unzip $ zipWith3 trasfer qs xs ys
	| otherwise = swap (correct qs) cx cy 


split :: Twin a -> [a] -> [a]
split f xs = z1 ++ z2 where
	(z1,z2) = uncurry zipSpl . splitAt (length xs `div` 2) $ xs	
	zipSpl [] [] = ([],[])
	zipSpl [] [x] = ([],[x])
	zipSpl (x:xs) (y:ys) = ((x:) *** (y:)) . zipSpl xs $ ys
	zipSpl (x:xs) [] = error "splittable broken"


incontro :: (Functor m, Num q, Ord q, MonadRandom m) => [q] -> [Carrello q] -> m [Carrello q]
incontro qs s = split (correct qs) `fmap` shuffleM s
	
	


	

