{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
-- | Una cassetta è il modello di commercio con il quale ogni attore si presenta al mercato. Essa contiene la merce che egli vende, acquista , e i giudizi che servono a valutarne gli acquisti.
module Cassetta where

import Tags
import Data.Binary
import Data.Tuple.HT
import Data.Monoid


data Richiesta l q a = Richiesta (Tags a Selettore) (l q) 


data Merce q a = Merce {
	prodotto :: Tags a Definizione,
	peso :: q
	} 


data Cassetta l q a = Cassetta {
	offerte :: [Merce q a],
	acquisti :: [Merce q a],
	richieste :: [Richiesta l q a]
	} 

pesata :: (Num q, Ord a) => Tags a Selettore -> [Merce q a] -> q
pesata sel = sum . map peso . selezione prodotto sel

class Monoid g => Giudizio l q g where
	valutazione :: l q -> q -> g

giudizio1 :: (Num q, Giudizio l q g, Ord a) => Richiesta l q a -> [Merce q a] -> g
giudizio1 (Richiesta sel l) = valutazione l . pesata sel 

giudizio  :: (Num q, Giudizio l q g , Ord a) => Cassetta l q a -> g
giudizio (Cassetta os as rs) = mconcat . map (flip giudizio1 as) $ rs




------------- Instances ------------------------------------------------


instance (Ord a, Binary (l q), Binary a) => Binary (Richiesta l q a) where
	put (Richiesta t l) = put (t,l)
	get = uncurry Richiesta `fmap` get
instance (Ord a, Binary q, Binary a) => Binary (Merce  q a) where
	put (Merce p q) = put (p,q)
	get = uncurry Merce `fmap` get
instance (Ord a, Binary q, Binary a, Binary (l q)) => Binary (Cassetta l q a) where
	put (Cassetta os as rs) = put (os, as, rs)
	get = uncurry3 Cassetta `fmap` get

