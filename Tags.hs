{-# LANGUAGE GADTs, FlexibleInstances #-}
-- | I Tags rappresentano le coordinate dei prodotti. Quando vogliamo selezionare un prodotto indichiamo un sottoinsieme dei suoi tags. Un insieme vuoto di tags seleziona qualsiasi prodotto.
-- Un valore Tags è  utilizzato come selettore o definizione, matematicamente identici sono tenuti separati per semantica. La funzione 'selezione' li distingue nella firma. 
module Tags (selezione, Tags, Selettore, Definizione, mkSelettore, mkDefinizione, addTag, removeTag) where


import Data.Set (Set, isSubsetOf, fromList, insert, delete)
import Data.Binary (Binary (..))

-- | Un segnaposto per i selettori. 
data Selettore
-- | Un segnaposto per le definizioni
data Definizione

-- | I tags sono matematicamente degli insiemi di elementi tra loro diversi
data Tags a t where
	Selettore :: Set a -> Tags a Selettore
	Definizione :: Set a -> Tags a Definizione

instance (Ord a, Binary a) => Binary (Tags a Selettore) where
	put (Selettore x) = put x
	get = Selettore `fmap` get
	
instance (Ord a, Binary a) => Binary (Tags a Definizione) where
	put (Definizione x) = put x
	get = Definizione `fmap` get

setOf (Definizione x) = x

-- | seleziona i prodotti che intercettano una scelta
selezione ::  (Ord a) =>  (b -> Tags a Definizione) -> Tags a Selettore -> [b] -> [b]
selezione defOf (Selettore x) = filter $ isSubsetOf x . setOf . defOf

-- | nuovo selettore da una lista di tags
mkSelettore :: Ord a => [a] -> Tags a Selettore
mkSelettore = Selettore . fromList

-- | nuova definizione da una lista di tags
mkDefinizione :: Ord a => [a] -> Tags a Definizione
mkDefinizione = Definizione . fromList


change :: Ord a => (a -> Set a -> Set a) -> a -> Tags a t -> Tags a t
change f a (Selettore s) = Selettore (a `f` s)
change f a (Definizione s) = Definizione (a `f` s)

addTag, removeTag :: Ord a => a -> Tags a t -> Tags a t
addTag = change insert
removeTag = change delete
