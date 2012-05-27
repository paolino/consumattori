{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances, StandaloneDeriving, DataKinds, GADTs, TypeFamilies #-}
module ProposteInstances () where

import Test.HUnit
import Data.Binary
import Data.DeriveTH
import Control.Monad (liftM2)

import Proposte

----------------------------------------------
-- istanze di classe -------------------------
----------------------------------------------

deriving instance Eq a => Eq (Filtro a)
deriving instance Show a => Show (Filtro a)
	
$(derive makeBinary ''Filtro)

deriving instance Eq a => Eq (Descrizione a b)
deriving instance Show a => Show (Descrizione a b)
instance (Ord (Prodotto a), Binary (Prodotto a)) => Binary (Descrizione (Prodotto a) Offerta) where
	put (Definizione s) = put s
	get = Definizione `fmap` get 

instance (Ord (Prodotto Domanda), Binary (Prodotto Domanda)) => Binary (Descrizione (Prodotto Domanda) Domanda) where
	put (Filtro f) = put f
	get = Filtro `fmap` get

[z,u,d,t] = [0,1,2,3] :: [Word8]

deriving instance Show q => Show (Limite q b p)
deriving instance Eq q => Eq (Limite q b p)
instance Binary q => Binary (Limite q Inferiore Domanda) where
	put Zero = put z
	put (Obbligatorio q) = put u >> put q
	put (Limitato q) = put d >> put q
	get = do	t <- getWord8
			case t of
				0 -> return Zero
				1 -> Obbligatorio `fmap` get
				2 -> Limitato `fmap` get
instance Binary q => Binary (Limite q Inferiore Offerta) where
	put = error "instance Binary q => Binary (Limite q Inferiore Offerta)"
	get = error "instance Binary q => Binary (Limite q Inferiore Offerta)"
instance Binary q => Binary (Limite q Superiore Domanda) where
	put (Limitato q) = put q
	get = Limitato `fmap` get

instance Binary q => Binary (Limite q Superiore Offerta) where
	put (Limitato q) = put z >> put q
	put (Quantificato n q) = put u >> put n >> put q
	get  = do	t <- getWord8
			case t of
				0 -> Limitato `fmap` get
				1 -> liftM2 Quantificato get get
	

deriving instance Show q => Show (Quantità q b)
deriving instance Eq q => Eq (Quantità q b)
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
deriving instance (Eq q, Eq a) => Eq (Proposta a q l)
instance (Binary a, Binary q, Binary (Descrizione a l), Binary (Quantità q l)) => Binary (Proposta a q l) where
	put (Proposta d es) = put d >> put es
	get = liftM2 Proposta get get


