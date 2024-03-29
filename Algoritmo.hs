{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DataKinds, GADTs, BangPatterns, FlexibleContexts #-}

module Algoritmo  where

import Prelude hiding (foldr, sum)
import Data.Array (Array, elems, (!))
import Data.Ix (Ix)
import Data.Ratio (Rational)
import Data.Traversable (mapAccumL)
import Data.Foldable (foldr, sum)

newtype Utente = Utente Int deriving (Ord,Ix,Eq, Num, Enum, Show)
newtype Prodotto = Prodotto Int deriving (Ord,Ix,Eq, Num, Show)

newtype Quantità = Quantità Rational deriving (Num, Ord, Eq, Show)


data Interesse = Positivo | Neutro | Negativo deriving (Enum)
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


newtype Matrice q = Matrice (DatiUtente (DatiProdotto q)) deriving (Show)

zipMatrice f = zipArray (zipArray f)

zipArray :: Ix i => (a -> b -> c) -> Array i a -> Array i b -> Array i c
zipArray f a = snd . mapAccumL (\(x:xs) y -> (xs, f x y)) (elems a)

type DatiUtente q = Array Utente q
type DatiProdotto q = Array Prodotto q

type Quanti = DatiProdotto Quantità


newtype Rapporto = Rapporto Rational deriving (Ord, Eq, Num, Show)



flex :: [Int] -> Bool
flex _ = False
-- flex [] = False
-- flex [_] = False
-- flex (x:y:_) = x > y
-- flex (vm:vx:vM:_) = if vx >= vm && vx >= vM

data Giudizio = Giudizio [Prodotto] (Quantità -> Bool)
newtype Giudizi = Giudizi (DatiUtente [Giudizio])

giudizi :: Giudizi ->  Matrice Quantità -> DatiUtente [Bool]
giudizi (Giudizi vg) (Matrice mq) = zipArray g vg mq where
	g :: [Giudizio] -> DatiProdotto Quantità -> [Bool]
	g gs aq = map (\(Giudizio ps fq) -> fq . sum . map (aq !) $ ps) gs

valutazione :: Giudizi ->  Matrice Quantità -> Int
valutazione vg mq = foldr f 0 (giudizi vg mq) where
	f :: [Bool] -> Int -> Int
	f bs n = n + length (filter id bs)


 



