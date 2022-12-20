Program Usage: 

$ stack ghc -- -threaded -rtsopts DPLL.hs
$ ./DPLL <filename> 1 +RTS -N8 -ls


Test Usage:
## can add test cases in Test.hs

$ stack ghc -- -threaded -rtsopts Test.hs
$ ./test



## CNF file format please refers to https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
## The line start with "p" is the program description of how many variables and clauses
## The line start with "c" is the comment line
## each line is a CNF clause ending with 0.
## note that the program can't parse some of the files in the website since 0 is not the same line with the clause in some files, so please
append 0 in the same line with the clause.



p cnf 3 2
1 -3 0
2 3 -1 0

is the clause 
[x1 (not x3) x2]
[x2 x3 (not x1)]
