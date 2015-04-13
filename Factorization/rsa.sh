echo InputNumber, $1
echo ExtraClausesLevel, $2
echo ModuloMode, $3
# IncByTwoToM_1_1:IncByTwoToM_1_2:Dec_1:Dec_2:EQ:
./FactorUsingSAT --Mode RSAFactoring --SatSolverType CNFCollection --OutputFilename bin.cnf --FactorizerMode BinaryRep --verbosity 0  --InputNumber $1 --ExtraClausesLevel $2
#./glucose-3 bin.cnf
./FactorUsingSAT --Mode RSAFactoring --SatSolverType CNFCollection --OutputFilename inc12.cnf --FactorizerMode ModuloRep --ModuloMode $3 --verbosity 0  --AddModuloMod IncByTwoToM_1 --ExtraClausesLevel  $2  --InputNumber $1
#./glucose-3 inc12.cnf
./FactorUsingSAT --Mode RSAFactoring --SatSolverType CNFCollection --OutputFilename dec12.cnf --FactorizerMode ModuloRep --ModuloMode $3 --verbosity 0  --AddModuloMod Decr --ExtraClausesLevel  $2  --InputNumber $1
#./glucose-3 dec12.cnf
head -n 3 bin.cnf inc12.cnf dec12.cnf
