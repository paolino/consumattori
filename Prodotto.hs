{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | I Tags rappresentano le coordinate dei prodotti. Quando vogliamo selezionare un prodotto indichiamo un sottoinsieme dei suoi tags. Un insieme vuoto di tags seleziona qualsiasi prodotto.
module Tags (selezione, Tags, mkTags) where


import Data.Set (Set, isSubsetOf, fromList)
import Data.Binary (Binary (..))


-- | I tags sono matematicamente degli insiemi di elementi tra loro diversi
newtype Tags a = Tags (Set a) deriving (Show, Read, Binary)

untags (Tags x) = x


-- | seleziona i prodotti che intercettano una scelta
selezione ::  (Ord a) =>  (b -> Tags a) -> Tags a -> [b] -> [b]
selezione tagsOf (Tags x) = filter $ isSubsetOf x . untags . tagsOf


-- | Un insieme di Tags
mkTags :: Ord a => [a] -> Tags a
mkTags = Tags . fromList
