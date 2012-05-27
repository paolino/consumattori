import Data.Map
import Proposte


data Utente = Interno | Esterno 
data Consegna a q = Consegna {
	interni :: Map (Definizione a Interno)  
	contenuto :: Map (Definizione a Offerta) q,
	valutazione :: [(Filtro a Domanda, Quantità q Domanda)]
	} 

type Errore q = Quantità q Domanda -> q -> Maybe q

valuta :: Errore q -> Consegna a q -> q
valuta e (Consegna c vs) = 
