  ValidArguments: array [1..13] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--FactorizerMode',
     '--ExtraClausesLevel',
     '--Verbosity',
     '--ModuloMode',
     '--AddaLeb',
     '--ExtraClausesMode',
     '--AddModuloMod',
     '--Mode',
     '--InputNumber',
     '--InputSize',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..13] of AnsiString=
    (
//--ModularEncoder:
   'DP:DCDirect:DCTseitin:CARD.DP:CARD.DC:CARD.SN:Less.Variables:'
     + 'SingleSorter.DP:SingleSorter.DC:SingleSorter.SN:BIN:END',
//     '--SATSolverType',
     'CNFCollection:InternalMiniSAT:CNFStream:END',
//     '--FactorizerMode',
     'BinaryRep:ModuloRep:END',
//     '--ExtraClausesLevel',
     'None:END',
//    Verbosity
     'None:END',
//     '--ModuloMode',
     'Mode1:Mode2:Mode3:Mode4:Mode5:UsingDP:END',
//     '--AddaLeb'
     'True:False:END',
//     '--ExtraClausesMode',
      'None:END', //Dec_1:Dec_2:IncByTwoToM_1_1:IncByTwoToM_1_2:EQ:END',
//     '--AddModuloMod',
      'Decr:IncByTwoToM_1:END',
//     '--Mode',
      'Factoring:RSAFactoring:Multiplication:END',
//     '--InputNumber',
      'None:END',
//     '--InputSize',
       'None:END',
//     '--OutputFilename'
     'None:END'
    );



