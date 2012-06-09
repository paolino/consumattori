{-# LANGUAGE GeneralizedNewtypeDeriving,TypeFamilies, FlexibleContexts, BangPatterns, DataKinds, GADTs #-}
-- | Implementazione Genetica da parte di Algoritmo
module Istanza where

import Prelude hiding (concatMap, mapM)

import System.Random (StdGen, split, Random (..))
import System.Random.Shuffle (shuffleM)
import Control.Monad.Random (Rand, MonadRandom, getRandom, runRand, evalRand)
import Control.Applicative ((<$>)) 
import Data.Array (listArray, array, elems, bounds, (!))
import Control.Arrow (second) 
import Data.Traversable (mapM)
import Control.Monad (ap, liftM2, replicateM)
import Control.Conditional ((??))
import Data.Foldable (concatMap)
import Data.Ratio (approxRational)
import Genetica
import Algoritmo

type instance Valore (Matrice Quantità) = Rapporto
type Operatori = Variazioni Soluzioni
type Consegna = Matrice Quantità
type Volontà = Matrice Interesse

type instance Contesto Operatori Consegna = Volontà
type instance Contenitore Soluzioni Consegna = []
type instance Contenitore Operatori Consegna = []
type instance Monade Operatori Consegna = Rand StdGen

newtype Limite = Limite Int deriving (Num,Ord,Eq)

data Parametri = Parametri 
	{ limite 	::  	Limite 	-- ^ numero massimo di iterazioni a caccia del massimo
	, quanti 	::	Quanti  -- ^ quantità unitarie di trasferimento
	, giudizi 	::	Giudizi -- ^ preferenze utenti
	, nconsegne	:: 	Int 	-- ^ larghezza strato soluzioni
	, noperatori	:: 	Int 	-- ^ larghezza strato operatori
	, sviluppo 	:: 	Int 	-- ^ potenza di riproduzione operatori
	}

instance Random Interesse where
	random g = let (x,g') = random g in (toEnum (x `mod` 3), g')
	randomR = undefined

mix :: (Functor m, MonadRandom m) => Int -> [a] -> m [(a,a)]
mix n xs = (zip `ap` drop (n `div` 2)) <$> shuffleM xs

-- | costruisce un operatore a partire dalle sue volontà, una matrice di 'Interesse', utente x prodotto
operatore 	:: Parametri 			-- ^ parametri comuni agli operatori
		-> Volontà 			-- ^ descrizione operatore
		-> Elemento Operatori Consegna 	-- ^ operatore di primo livello

operatore (Parametri l qs gs _ _ _) (Matrice mi) = Operatore (Matrice mi) f where 
		f (Soluzione mq) =  second Soluzione <$> corri (Limite 0) [valutazione gs mq] mq 
		(1,u) = bounds mi
		corri :: Limite -> [Rapporto] -> Consegna -> Rand StdGen (Rapporto,Consegna)
		corri !n pvs mq@(Matrice aq) = do
			ijs 	<- mix (fromEnum u) [1 .. u]
			let 	nmq 	= Matrice $ array (bounds mi) $ concatMap coppie ijs 
				nvs 	= valutazione gs nmq : pvs
				miq 	= zipArray (zipArray (,)) mi aq
				coppie (i,j) = [(i',listArray (bounds qs) mi'), (j',listArray (bounds qs) mi'')] 
					where	(i',j') 	= if i <= j then (i,j) else (j,i)
						(mi',mi'') 	= unzip $ zipWith3 trasfer (elems qs) (elems $ miq ! i') (elems $ miq ! j')
			if	flex nvs || n >= l 
				then 	return (nvs !! 1, mq) 
				else  	corri (n + 1) nvs nmq 
			



crescita		::   	Parametri
			-> 	Int	-- ^ numero di incroci, ognuno produce aggiunge mezza popolazione di operatori nuovi operatori 
			-> 	Crescita Operatori Consegna
crescita  p k = Crescita (f k) where
	f l xs 
		| l <= 0 = return xs	
		| otherwise = liftM2 (++) (mix r xs >>= mapM g) (f (l - 1) xs)
		where	r = length  xs
	g (Operatore (Matrice i) _, Operatore (Matrice j) _) = (operatore p . Matrice . zipMatrice (flip ($)) i) <$> mapM (mapM f) j where
			f x = (id ?? const x) <$> (getRandom :: Rand StdGen Bool) 


data Algoritmo = Algoritmo 
	{	procedi :: Algoritmo
	,	consegne :: [(Rapporto, Consegna)]
	}

algoritmo :: StdGen -> Parametri -> Consegna -> Algoritmo
algoritmo s p@(Parametri _ _ gs nsols nops k) c0@(Matrice qs) = let 
	nuovo s sols ops = Algoritmo (nuovo s' sols' ops') $ map (second $ \(Soluzione s) -> s) sols' where
			((ops',sols'), s') = flip runRand s $ passo (taglioClassico nsols) (taglioClassico nops) (crescita p k) (ops,sols)
	(s',s'') = split s
	in nuovo s' [(valutazione gs c0, Soluzione c0)] . flip evalRand s'' $ replicateM nops (operatore p . Matrice <$> mapM (mapM $ const getRandom) qs)
		
mkConsegna :: Float -> [[Float]] -> Consegna
mkConsegna epsilon ns = let
	u = Utente $ length ns
	p = Prodotto $ minimum (map length ns)
	in Matrice $ listArray (1,u) (map (listArray (1,p) . map (Quantità . flip approxRational epsilon)) $ ns)                  

data Limiti = Massimo Float [Int] | Minimo Float [Int] | Facoltativo Float [Int]

mkGiudizi :: Float -> [[Limiti]] -> Giudizi
mkGiudizi epsilon xs = Giudizi . listArray (1, Utente u) . map (map f) $ xs where
	u = length xs
	f (Massimo y is) = mkGiudizio is y (<=)
	f (Minimo y is) = mkGiudizio is y (>=)
	f (Facoltativo y is) = mkGiudizio is y (\y x -> x <=0 || x >= y)
	mkGiudizio is y f = Giudizio (map Prodotto is) (f . Quantità . approxRational epsilon $y)

