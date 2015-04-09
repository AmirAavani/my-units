# for m in {2..10}; 
n=$1
#    n=`expr $m \* $m \* 1000 + 1`
    #echo $n; 
    #./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.PrimeBin.cnf --FactorizerMode Modulo.Prime.Binary --Verbosity 15 > $n.PrimeBin.out;   
    #./minisat_static $n.PrimeBin.cnf $n.PrimeBin.ans >$n.sat.PrimeBin.out 2>$n.sat.PrimeBin.err ; grep UNSAT $n.sat.PrimeBin.out>/dev/null; PrimeBinRes=$?

#./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.noaLEb.cnf --FactorizerMode BinaryRep --Verbosity 15 --AddaLEb False > $n.BinRep.noaLEb.out;   
#./minisat_static $n.BinRep.noaLEb.cnf $n.BinRep.noaLEb.ans >$n.BinRep.sat.noaLEb.out 2>$n.BinRep.sat.noaLEb.err ; grep UNSAT $n.BinRep.sat.noaLEb.out >/dev/null; noaLEb_BinRes=$?
./FactorUsingSAT --InputNumber $n --SatSolverType CNFCollection --OutputFileName $n.BinRep.aLEb.cnf --FactorizerMode BinaryRep --Verbosity 15 --AddaLEb True > $n.BinRep.aLEb.out;   
./minisat_static $n.BinRep.aLEb.cnf $n.BinRep.aLEb.ans  2>$n.BinRep.sat.aLEb.err ; while read VAR4; do echo $VAR4; done
grep UNSAT $n.BinRep.sat.aLEb.out >/dev/null; aLEb_BinRes=$?
#if [[ $noaLEb_BinRes -eq 0 ]];
#then
  #echo $PrimeBinRes, $BinRes
  #echo $n
echo $n, $aLEb_BinRes
#fi
