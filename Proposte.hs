{-# LANGUAGE  DataKinds, GADTs, KindSignatures,  FlexibleContexts #-}

module Proposte  where

import Data.Set

-- | linguaggio di programmazione filtri
data Filtro a = Aperto | Includi a | Escludi a | Oppure (Filtro a) (Filtro a) | Inoltre (Filtro a) (Filtro a) 

-- | controlla la passabilità di un insieme di tag per un filtro
test :: Ord a => Set a -> Filtro a -> Bool
test s Aperto = True
test s (Includi x) = x `member` s
test s (Escludi x) = not $ x `member` s
test s (Oppure x y) = test s x  || test s y
test s (Inoltre x y) = test s x  && test s y 

-- | distinzione tra l'apporto di merce ed il prelievo
data TipoProposta = Offerta | Domanda

-- | una descrizione è un insieme di valori, distinguiamo con il verso i valori che definiscono le merci e i valori che li filtrano 
data Descrizione :: * -> TipoProposta -> * where 
	Definizione :: Ord a => Set a -> Descrizione a Offerta
	Filtro :: Ord a => Filtro a -> Descrizione a Domanda

-- | tipi di limite
data TipoLimite = Inferiore | Superiore

-- | valori di limite
data Limite :: * -> TipoLimite -> TipoProposta -> * where
	Quantificato :: Int -> q -> Limite q Superiore Offerta 
	--  limite generico
	Limitato :: q -> Limite q l p
	--  limite inferiore assente
	Zero :: Limite q Inferiore Domanda
	--  limite inferiore non nullo obbligatorio
	Obbligatorio :: q -> Limite q Inferiore Domanda
	

erroreLimiteInferiore :: (Ord q, Num q) => q -> Limite q Inferiore p -> Maybe q
erroreLimiteInferiore x (Limitato y) 
	| x == 0 = Nothing
	| x < y = Just (y - x)
	| otherwise = Nothing

erroreLimiteInferiore x Zero = Nothing
erroreLimiteInferiore x (Obbligatorio y) 
	| x > y = Nothing
	| otherwise = Just (y - x)

erroreLimiteSuperiore :: (Ord q, Num q) => q -> Limite q Superiore p -> Maybe q
erroreLimiteSuperiore x (Limitato y)
	| x > y = Just (x - y)
	| otherwise = Nothing
erroreLimiteSuperiore x (Quantificato n y) = erroreLimiteSuperiore x (Limitato $ fromIntegral n * y)

-- | valutazioni sulle quantità. Per la domanda sono esprimibili sia limite inferiore che superiore, per l'offerta solo il superiore. L'opzione 'Assente' vale per tutti e due i casi e annulla le quantità.
data Quantità :: * -> TipoProposta -> *  where 
	Acquisto :: Limite q Inferiore Domanda -> Limite q Superiore Domanda -> Quantità q Domanda
	Vendita :: Limite q Superiore Offerta -> Quantità q Offerta
	Assente :: Quantità q l


-- | proposte di offerta e domanda complete. 
data Proposta :: * -> * -> TipoProposta -> *  where
	Proposta :: Descrizione a l -> [Quantità q l] -> Proposta a q l


---------------------------------------------------
--------------------- parser ----------------------
---------------------------------------------------



