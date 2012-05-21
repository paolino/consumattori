{-# LANGUAGE TemplateHaskell,NoMonomorphismRestriction, DataKinds, GADTs, KindSignatures, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}

module P where

import Data.DeriveTH
import Data.Set
import Data.Binary
import Control.Monad (liftM2)

-- | linguaggio di programmazione filtri
data Filtro a = Aperto | Include a | Esclude a | Oppure (Filtro a) (Filtro a) | Inoltre (Filtro a) (Filtro a) 

-- | controlla la passabilità di un insieme di tag per un filtro
test :: Ord a => Set a -> Filtro a -> Bool
test s Aperto = True
test s (Include x) = x `member` s
test s (Esclude x) = not $ x `member` s
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
data Limite :: * -> TipoLimite -> * where
	--  limite generico
	Limitato :: q -> Limite q l
	--  limite inferiore assente
	Zero :: Limite q Inferiore
	--  limite inferiore non nullo obbligatorio
	Obbligatorio :: q -> Limite q Inferiore
	

erroreLimiteInferiore :: (Ord q, Num q) => q -> Limite q Inferiore -> Maybe q
erroreLimiteInferiore x (Limitato y) 
	| x == 0 = Nothing
	| x < y = Just (y - x)
	| otherwise = Nothing

erroreLimiteInferiore x Zero = Nothing
erroreLimiteInferiore x (Obbligatorio y) 
	| x > y = Nothing
	| otherwise = Just (y - x)

erroreLimiteSuperiore :: (Ord q, Num q) => q -> Limite q Superiore -> Maybe q
erroreLimiteSuperiore x (Limitato y)
	| x > y = Just (x - y)
	| otherwise = Nothing

data Quantità :: * -> TipoProposta -> *  where 
	Acquisto :: Limite q Inferiore -> Limite q Superiore -> Quantità q Domanda
	Vendita :: Limite q Superiore -> Quantità q Offerta
	Assente :: Quantità q l


-- | proposte di offerta e domanda complete
data Proposta :: * -> * -> TipoProposta -> *  where
	Proposta :: Descrizione a l -> [Quantità q l] -> Proposta a q l

pr = Proposta (Definizione $ fromList [1,2,3]) [Assente, Assente, Vendita (Limitato 10), Vendita (Limitato 12), Vendita (Limitato 8)]
ri = Proposta (Filtro $ Include 1) [Assente, Acquisto Zero (Limitato 3), Acquisto (Limitato 4) (Limitato 3), Acquisto (Obbligatorio 3) (Limitato 5)]


----------------------------------------------
-- istanze di classe -------------------------
----------------------------------------------

deriving instance Eq a => Eq (Filtro a)
deriving instance Show a => Show (Filtro a)
	
$(derive makeBinary ''Filtro)

deriving instance Eq a => Eq (Descrizione a b)
deriving instance Show a => Show (Descrizione a b)
instance (Ord a, Binary a) => Binary (Descrizione a Offerta) where
	put (Definizione s) = put s
	get = Definizione `fmap` get 

instance (Ord a, Binary a) => Binary (Descrizione a Domanda) where
	put (Filtro f) = put f
	get = Filtro `fmap` get

[z,u,d,t] = [0,1,2,3] :: [Word8]

deriving instance Show q => Show (Limite q b)
instance Binary q => Binary (Limite q Inferiore) where
	put Zero = put z
	put (Obbligatorio q) = put u >> put q
	put (Limitato q) = put d >> put q
	get = do	t <- getWord8
			case t of
				0 -> return Zero
				1 -> Obbligatorio `fmap` get
				2 -> Limitato `fmap` get
instance Binary q => Binary (Limite q Superiore) where
	put (Limitato q) = put q
	get = Limitato `fmap` get
	

deriving instance Show q => Show (Quantità q b)
instance Binary q => Binary (Quantità q Domanda) where
	put (Acquisto li ls) = put z >> put li >> put ls
	put Assente = put u 
	get = do	t <- getWord8
			case t of
				0 -> liftM2 Acquisto get get
				1 -> return Assente

instance Binary q => Binary (Quantità q Offerta) where
	put (Vendita ls) = put z >> put ls
	put Assente = put u 
	get = do	t <- getWord8
			case t of
				0 -> Vendita `fmap` get
				1 -> return Assente

deriving instance (Show q, Show a) => Show (Proposta a q l)
instance (Binary a, Binary q, Binary (Descrizione a l), Binary (Quantità q l)) => Binary (Proposta a q l) where
	put (Proposta d es) = put d >> put es
	get = liftM2 Proposta get get


