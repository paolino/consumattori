{-# LANGUAGE ScopedTypeVariables #-}
import Text.CSV
import Algoritmo
import Istanza


import Control.Exception
import Data.Ratio
import Data.Array
import Data.List
import Data.Ord
import Control.Arrow
import Data.Function (on)
import System.Random
import Text.Printf



parseCas ::  Float -> Int -> Int -> [[String]] -> Matrice Quantità
parseCas app u p = Matrice . listArray (1,Utente u) . map (listArray (1,Prodotto p)  . map (Quantità . flip approxRational app . read))

parsePref :: [String] -> (Utente, Limiti)
parsePref (su:tp:sw:ts) 
	| tp == "Massimo" =  (Utente $ read su, Massimo (read sw) (map read ts))
	| tp == "Minimo" =  (Utente $ read su, Minimo (read sw) (map read ts))
	| tp == "Facoltativo" =  (Utente $ read su, Facoltativo (read sw) (map read ts))

op :: Maybe (Run, [[String]] -> Bool) -> IO ()
op wrl = do 
	srl <- parseCSVFromFile "algoritmo.csv"
	let ciclo (Run run) t = run >>= \run' -> op (Just (run',t))
	case srl of 
		Left e -> print e >> case wrl of 
			Nothing -> return () 
			Just (run, t)  -> ciclo run t
		Right srl -> 
			let parsing f = do 
				prl <- parse srl 
				case prl of 
					Left e -> print e >> f
					Right run -> ciclo run (== srl)
			in case wrl of 
				Nothing -> parsing (return ()) 
				Just (run, t) -> case t srl of
					True -> ciclo run t
					False -> parsing (ciclo run t)

newtype Run = Run (IO Run)

parse :: [[String]] -> IO (Either String Run)
parse r = handle (\(_ :: SomeException) -> print "ahi" >> return (Left "problemi nella lettura dei valori")) $ do
	let 	hs:sqs:rs = map (filter (not.null)) r
		[u,p,pr,it,ns,no,psv] = map read hs
		(cas,rs') = splitAt u rs
		(prefs,_) = splitAt pr rs'
		qs = map read sqs
		app = minimum qs / 2
		param = Parametri (Limite it) (listArray (1,Prodotto p) $ map (Quantità . flip approxRational app) qs) 
				(mkGiudizi app (Utente u) $ map parsePref prefs) 
				ns no psv
	
	s <- getStdGen
	let 	al = algoritmo s param $ parseCas app u p cas
		run al = report (Algoritmo.giudizi $ Istanza.giudizi param) al >>= return . Run . run
	return $ Right $ Run $ run al
			

report g (Algoritmo  q ((rfi,Matrice m):_)) =  do
	putStr "\nSoddisfatti: " >> print rfi
	let 	gs = g (Matrice m)
		w = zipArray (,) gs m
	let d (Utente i,(gs,xs)) = do
		printf "%2d) " i
		mapM_ (printf "%5.1f") . map (\(Quantità x) -> fromRational x :: Float) . elems $ xs
		putStr " "
		mapM_ (printf "%2s") . map ((:[]) . head . show)  $ gs
		putStrLn ""
	mapM_ d $ assocs w
	return q

main = op Nothing 
