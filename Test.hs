import DPLL
import qualified Data.Set as Set
import qualified Data.Map as Map

{--

$ stack ghc -- -threaded -rtsopts Test.hs
$ ./test

--}


-- satisfiable
clauses1  :: Clauses
clauses1 = [[Lit "1", Not "2", Lit "3"], [Lit "1", Lit "3"], [Not "1", Not "2"], [Lit "3"]]
symbols1 = Set.fromList ["1","2","3"]
result1 = True

-- not satisfiable
clauses2  :: Clauses
clauses2 = [[Lit "x"], [Not "x"]]
symbols2 = Set.fromList ["x"]
result2 = False


--satisfiable
clauses3  :: Clauses
clauses3 = [[Lit "1", Not "2", Lit "3"], [Lit "1", Lit "2"], [Lit "3"]]
symbols3 = Set.fromList ["1","2","3"]
result3 = True


--satisfiable
clauses4  :: Clauses
clauses4 = [[Lit "1", Not "2"], [Lit "2", Lit "3"], [Not "3", Lit "2"], [Not "3"], [Lit "1"]]
symbols4 = Set.fromList ["1","2","3"]
result4 = True



tests_clause = [clauses1, clauses2, clauses3, clauses4]
tests_symbols = [symbols1, symbols2, symbols3, symbols4]
tests_results = [result1, result2, result3, result4]

main :: IO ()
main = runtest tests_clause tests_symbols tests_results
    where runtest tests_clause tests_symbols tests_results = 
            case (tests_clause, tests_symbols, tests_results) of
                    ([],[],[]) -> return ()
                    (cs:crest, s:srest, r:rrest) -> do
                                                        case dpll_eval2 s cs Map.empty of
                                                            Just _ -> do 
                                                                        putStrLn $ show (r == True)
                                                                        runtest crest srest rrest
                                                            Nothing -> do
                                                                        putStrLn $ show (r == False)
                                                                        runtest crest srest rrest
                    _ -> do putStrLn "unexpected length of test, clause, results"
                            return ()
                                                                    
                            

