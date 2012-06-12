{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, DataKinds, ScopedTypeVariables, MonoLocalBinds, FlexibleInstances, UndecidableInstances#-}

module Genetica where
	
import Prelude hiding (mapM, sum)
import Data.Traversable (mapAccumL, Traversable,mapM, forM)
import Data.Monoid  (mconcat, Monoid)
import Data.Foldable (toList, sum)
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.DeepSeq
import Control.Parallel.Strategies

data Strato = Soluzioni | Variazioni Strato 

type family Valore a :: *
type family Contesto (n :: Strato) a :: *
type family Monade (n :: Strato) a :: * -> *

type Valutato b a = (Valore a, b a)

data Elemento :: Strato -> * -> * where
	Soluzione :: a -> Elemento Soluzioni a
	Operatore :: Contesto (Variazioni n) a -> (Elemento n a -> Monade (Variazioni n) a (Valutato (Elemento n) a)) -> Elemento (Variazioni n) a

instance (NFData a) => NFData (Elemento Soluzioni a) where
	rnf (Soluzione x) = rnf x

instance (NFData (Contesto (Variazioni n) a), NFData a) => NFData (Elemento (Variazioni n) a) where
	rnf (Operatore c _) = rnf c

type family Contenitore (n :: Strato) a :: * -> *
type StratoValutato n a = Contenitore n a (Valutato (Elemento n) a)

operazione :: forall a n m . (Monad (Monade (Variazioni n) a) , Num (Valore a), Traversable (Contenitore n a))
	=> StratoValutato n a
	-> Elemento (Variazioni n) a
	-> Monade (Variazioni n) a (Valore a, StratoValutato n a)

operazione es (Operatore _ o) =  do 
	ns <- mapM (o . snd)  es 
	return (sum (fst `fmap` ns) - sum (fst `fmap` es), ns)
		
type Variazione a = a -> a
type VariazioneM m a = a -> m a
	
newtype Taglio n a = Taglio (Variazione (StratoValutato n a))

taglioClassico :: (Ord (Valore a), Contenitore n a ~ []) => Int -> Taglio n a
taglioClassico n = Taglio $ take n . sortBy (flip $ comparing fst) 


type StratoInvalutato n a = Contenitore n a (Elemento n a)

newtype Crescita n a = Crescita (VariazioneM (Monade n a) (StratoInvalutato n a))

passo 	:: 	( Num (Valore a), Monad (Monade (Variazioni n) a)
		, Monoid (StratoValutato n a), Traversable (Contenitore n a)
		, Traversable (Contenitore (Variazioni n) a)
		)
	=> Taglio n a
	-> Taglio (Variazioni n) a
	-> Crescita (Variazioni n) a
	-> VariazioneM (Monade (Variazioni n) a) (StratoInvalutato (Variazioni n) a, StratoValutato n a)
passo (Taglio ps) (Taglio po) (Crescita g) (os, es) = do
	qs <- forM os $ \o -> do
		(v,ns) <- operazione es o
		return ((v,o),ns)
	ops <- g . fmap snd . po . fmap fst $ qs
	sols <- return . ps . mconcat . ( ++ [es]) . toList . fmap snd $ qs
	return (ops, sols)

passoPar :: ( Num (Valore a), Monad  (Monade (Variazioni n) a)
		, Monoid (StratoValutato n a), Traversable (Contenitore n a)
		, Traversable (Contenitore (Variazioni n) a)
		, NFData a, NFData (Valore a), NFData (Elemento n a) 
		)
	=> Taglio n a
	-> Taglio (Variazioni n) a
	-> Crescita (Variazioni n) a
	-> VariazioneM (Monade (Variazioni n) a) (StratoInvalutato (Variazioni n) a, StratoValutato n a)
passoPar (Taglio ps) (Taglio po) (Crescita g) (os, es) = do
	qs <- forM os $ \o -> do
		(v,ns) <- operazione es o
		return ((v,o),ns)
	ops <- g . fmap snd . po . fmap fst $ qs
	sols <- return . ps . mconcat . ( ++ [es]) . parMap (parTraversable rdeepseq) id . toList . fmap snd $ qs
	return (ops, sols)

