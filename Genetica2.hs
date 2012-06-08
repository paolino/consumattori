{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, DataKinds, ScopedTypeVariables, MonoLocalBinds #-}

module Genetica2 where
	
import Prelude hiding (mapM, sum)
import Data.Traversable (mapAccumL, Traversable)
import Data.Monoid  (mconcat, Monoid)
import Data.Foldable (toList)

data Strato = Soluzioni | Variazioni Strato

type Variazioni' = Variazioni Soluzioni
type Variazioni'' = Variazioni Variazioni'

type family Valore a
type Valutato b a = (Valore a, b a)

data Elemento :: Strato -> * -> * where
	Soluzione :: a -> Elemento Soluzioni a
	Operatore :: (Elemento n a -> (Elemento (Variazioni n) a, Valutato (Elemento n) a)) -> Elemento (Variazioni n) a

type family Contenitore a :: * -> *

type Inferiore n a = Contenitore (Elemento n a) (Valutato (Elemento n) a)

operazione :: forall a n. (Num (Valore a), Traversable (Contenitore (Elemento n a)))
	=> Inferiore n a
	-> Elemento (Variazioni n) a
	-> (Valutato (Elemento (Variazioni n)) a, Inferiore n a)

operazione es o =  mapAccumL operazione' (0,o) es where
	operazione' :: Valutato (Elemento (Variazioni n)) a -> Valutato (Elemento n) a -> (Valutato (Elemento (Variazioni n)) a, Valutato  (Elemento n) a)
	operazione' (vl, Operatore f) (v,e) = ((vl + v' - v,o'), w) where (o', w@(v' ,e')) =  f e


type Variazione a = a -> a

newtype Taglio n a = Taglio (Variazione (Contenitore (Elemento n a) (Valutato (Elemento n) a)))

type Superiore n a = Contenitore (Elemento (Variazioni n) a) (Elemento (Variazioni n) a)

newtype Crescita n a = Crescita (Variazione (Superiore n a))


passo :: (Num (Valore a), Monoid (Inferiore n a), Traversable (Contenitore (Elemento n a)), Traversable (Contenitore (Elemento (Variazioni n) a)))
	=> Taglio n a
	-> Taglio (Variazioni n) a
	-> Crescita n a
	-> Variazione (Superiore n a, Inferiore n a)
passo (Taglio ps) (Taglio po) g (os, es) = (\x -> (eops x, esols x)) .  fmap (operazione es) $ os where
	eops = fmap snd . po . fmap fst
	esols = ps . mconcat . (es:) . toList . fmap snd
