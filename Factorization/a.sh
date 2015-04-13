echo $1
./FactorUsingSAT --Mode Factoring --SatSolverType CNFCollection --OutputFilename bin.cnf --FactorizerMode BinaryRep --verbosity 32  --InputSize $1
./FactorUsingSAT --Mode Factoring --SatSolverType CNFCollection --OutputFilename inc12.cnf --FactorizerMode ModuloRep --ModuloMode Mode4 --verbosity 32  --AddModuloMod IncByTwoToM_1 --ExtraClausesLevel  IncByTwoToM_1_1:IncByTwoToM_1_2  --InputSize $1
head bin.cnf inc12.cnf
