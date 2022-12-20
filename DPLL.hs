module DPLL (
    processFile,
    startdpll,
    dpll,
    dpll_eval2,
    Clauses,
    Symbols,
    CNF,
    Lit(..)
)
where

import qualified Data.PQueue.Min as MinQ
import Data.Maybe ( mapMaybe )
import Data.List (find, foldr)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Parallel.Strategies hiding (parMap)
import System.IO(withFile, IOMode(ReadMode), hGetContents, hPutStrLn, stderr)
import System.Exit(exitFailure)
import System.Environment(getArgs, getProgName)
import Control.Monad (guard)

data Lit =
      Lit String
    | Not String
  deriving (Show, Eq)

type CNF = [Lit]
  

type Clauses = MinQ.MinQueue CNF


instance Ord CNF where
    


type Symbols = Set.Set String


type M = Map.Map String Bool


clauses1  :: Clauses
clauses1 = [[Lit "1", Not "2", Lit "3"], [Lit "1", Lit "3"], [Not "1", Not "2"], [Lit "3"]]
symbols1 = Set.fromList ["1","2","3"]

clauses2  :: Clauses
clauses2 = [[Lit "x"], [Not "x"]]
symbols2 = Set.fromList ["x"]



main :: IO ()
main = do args <- getArgs
          case args of
            [filename, para] ->
              processFile filename para
            _ -> do
                 pn <- getProgName
                 hPutStrLn stderr $ "Usage: "++pn++" <filename>" 
                 exitFailure



processFile :: String -> String -> IO ()
processFile filename para = 
    do withFile filename ReadMode (\h -> do contents <- hGetContents h 
                                            let (symbols, clauses) = (processCNF . filterComment) $ lines contents
                                            case para of
                                                "1" -> startdpll symbols clauses True
                                                _ -> startdpll symbols clauses False)
        where filterComment = filter (\x -> (head $ words x) /= "c" && (head $ words x) /= "p")
    

processCNF :: [String] -> (Symbols, Clauses)
processCNF lines = let symbolandclause = map processline lines in
                   let symbols = foldr (\(a,b) acc -> Set.union a acc) Set.empty symbolandclause in
                   let clause = foldr (\(a,b) acc -> b : acc) [] symbolandclause in
                    (symbols, clause)
  where processline line = let symbols = filter (\x -> x /= "0") (words line) in
                            let symbolClauses = map processSymbol symbols in 
                            let symbolset = foldr (\(a,b) acc -> Set.insert a acc) Set.empty symbolClauses in
                            let clauses = foldr (\(a,b) acc -> b : acc) [] symbolClauses in
                            (symbolset, clauses)
        processSymbol symbol
                     | head symbol == '-' = (tail symbol, Not (tail symbol))
                     | otherwise = (symbol, Lit symbol)



startdpll :: Symbols -> Clauses -> Bool -> IO ()
startdpll s c para = do 
    case para of
        True -> case (dpll_eval2 s c model) of
                  Just m -> putStrLn $ show m
                  Nothing -> putStrLn "unsat"
        _ -> case (dpll s c model) of
                  Just m -> putStrLn $ show m
                  Nothing -> putStrLn "unsat"
    where model = Map.empty


parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do b <- rpar (f a)
                     bs <- parMap f as 
                     return (b:bs)

dpll_eval2 :: Symbols -> Clauses -> M -> Maybe M
dpll_eval2 symbols cs m 
    | all (\x -> x == True) $ runEval $ parMap (isTrueInCNF m) cs = Just m
    | any (\x -> x == True) $ runEval $ parMap (isFalseInCNF m) cs = Nothing
    | otherwise = do case pures of
                        l@(x:xs) -> 
                            let newm = foldr (\(s,b) acc -> Map.insert s b acc) m pures in
                            let unassigned = foldl (\s x -> Set.delete (fst x) s) symbols pures in
                                  dpll_eval2 unassigned cs newm
                        _ -> case findUnit symbols cs m of
                                Just (s, c, m) ->
                                  dpll_eval2 s c m
                                Nothing -> 
                                   runEval $ do
                                        let ele = Set.elemAt 0 symbols
                                        i <- rpar $ dpll_eval2 (Set.delete ele symbols) cs (Map.insert ele True m)
                                        j <- rpar $ dpll_eval2 (Set.delete ele symbols) cs (Map.insert ele False m)
                                        rseq i
                                        case i of
                                            Just m -> do return (Just m)
                                            Nothing -> do rseq j
                                                          return j
    where pures = findPure symbols cs

dpll :: Symbols -> Clauses -> M -> Maybe M
dpll symbols cs m 
    | all (isTrueInCNF m) cs = Just m
    | any (isFalseInCNF m) cs = Nothing
    | otherwise = case pures of
                    l@(x:xs) -> dpll unassigned cs new_model
                    _ -> case findUnit symbols cs m of
                            Just (s, c, m) ->
                                dpll s c m
                            Nothing -> 
                                case dpll new_symbols cs (Map.insert one_element True m) of
                                    Just m -> Just m
                                    Nothing -> dpll new_symbols cs (Map.insert one_element False m)
    where pures = findPure symbols cs
          new_model = foldr (\(s,b) acc -> Map.insert s b acc) m pures
          unassigned = foldl (\s x -> Set.delete (fst x) s) symbols pures
          one_element = Set.elemAt 0 symbols
          new_symbols = Set.delete one_element symbols



findPure :: Symbols -> Clauses -> [(String,Bool)]
findPure s clause = 
        posassigns ++ negassigns
    where clausesHavesymbol symbol clause = mapMaybe (ifSymbolInCNF symbol) clause
          ifSymbolInCNF symbol cnf =
           case find (\x -> getSymbol x == symbol) cnf of
             Just lit -> Just (getSign lit)
             Nothing -> Nothing
          isAllTrue sp = all (==True) sp
          isAllFalse sp = all (==False) sp
          sl = Set.toList s
          possymbols = filter (\x -> isAllTrue $ clausesHavesymbol x clause) sl
          negsymbols = filter (\x -> isAllFalse $ clausesHavesymbol x clause) sl
          posassigns = runEval $ parMap (\x -> (x, True)) possymbols 
          negassigns = runEval $ parMap (\x -> (x, False)) negsymbols


getSymbol :: Lit -> String
getSymbol (Lit s) = s
getSymbol (Not s) = s

getSign :: Lit -> Bool
getSign (Lit _) = True
getSign (Not _) = False

applyNot :: Lit -> Lit
applyNot (Lit s) = Not s
applyNot (Not s) = Lit s 



findUnit :: Symbols -> Clauses -> M -> Maybe (Symbols, Clauses, M)
findUnit s c m = 
    let clauses = unitifyClauses m c in
    let unit_clauses = filter (\x -> length x == 1 && isNotInM (getSymbol $ head x) m) clauses `using` parList rpar in
        case unit_clauses of
            [] -> Nothing
            unit ->
                let first = head unit in
                let newm = Map.insert (getSymbol $ head first) (getSign $ head first) m in
                let symbols = Set.delete (getSymbol $ head first) s in
                let simple_clause = simplify clauses first in
                    Just (symbols, simple_clause, newm)
    where isNotInM symbol m =
           case Map.lookup symbol m of
            Just _ -> False
            Nothing -> True
          simplify clauses unit_clause = 
            filter (\x -> not (hasunit_clause x unit_clause)) clauses
          hasunit_clause cnf unit_clause =
            case find (\x -> x == head unit_clause) cnf of
              Just _ -> True
              _ -> False


        
unitifyClauses :: M -> Clauses -> Clauses
unitifyClauses m clauses =
    map unifyCNF clauses
    where unifyCNF cnf = case getunitassign cnf of
                           Just (x,_) -> [x]
                           Nothing -> cnf
          getunitassign cnf = getIfone $ (filter (\(x,b) -> b) (map findmodel cnf))
          findmodel l@(Lit s) = case Map.lookup s m of
                               Just b -> (l,b)
                               Nothing -> (l,True)
          findmodel l@(Not s) = case Map.lookup s m of
                               Just b -> (l,not b)
                               Nothing -> (l,True)
          getIfone [x] = Just x
          getIfone _ = Nothing
        


isTrueInCNF :: M -> CNF -> Bool
isTrueInCNF m cnf =
    or (mapMaybe g cnf)
  where g (Lit s) = Map.lookup s m
        g (Not s) = not <$> Map.lookup s m


isFalseInCNF :: M -> CNF -> Bool
isFalseInCNF m cnf = all not (map g cnf)
    where g (Lit s) = Map.findWithDefault True s m
          g (Not s) = not $ Map.findWithDefault False s m