{-# LANGUAGE DataKinds #-}

import Proposte 
import ProposteInstances 

import Data.Set
import Test.HUnit
import Data.Binary


----------------------------------
-- esempi ----------------------
--------------------------------

pr :: Proposta Int Float Offerta
pr = Proposta (Definizione $ fromList [1,2,3]) [Assente, Assente, Vendita (Quantificato 10 2), Vendita (Quantificato 8 2.5), Vendita (Quantificato 10 2), Vendita (Limitato 12)]
ri :: Proposta Int Float Domanda
ri = Proposta (Filtro $ Includi 1) [Assente, Acquisto Zero (Limitato 3), Acquisto (Limitato 4) (Limitato 3), Acquisto (Obbligatorio 3) (Limitato 5)]



t1 x = (decode . encode $ x) == x


