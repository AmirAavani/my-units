  ValidArguments: array [1..16] of AnsiString=
    ('--ModularEncoder',
     '--SATSolverType',
     '--FactorizerMode',
     '--MultiplierMode',
     '--Verbosity',
     '--ModuloMode',
     '--AddaLeb',
     '--ExtraClausesMode',
     '--AddModuloMod',
     '--Mode',
     '--InputNumber',
     '--a',
     '--b',
     '--c',
     '--InputSize',
     '--OutputFilename'
    );
  ValidArgumentsValues: array [1..16] of AnsiString=
    (
//--ModularEncoder:
   'DP:DCDirect:DCTseitin:CARD.DP:CARD.DC:CARD.SN:Less.Variables:'
     + 'SingleSorter.DP:SingleSorter.DC:SingleSorter.SN:BIN:END',
//     '--SATSolverType',
     'CNFCollection:InternalMiniSAT:CNFStream:END',
//     '--FactorizerMode',
     'BinaryRep:ModuloRep:END',
//   '--MultiplierMode'
     'BinaryRep:ModuloRep:END',
//    Verbosity
     'None:END',
//     '--ModuloMode',
     'Mode1:Mode2:Mode3:Mode4:Mode5:UsingDP:END',
//     '--AddaLeb'
     'True:False:END',
//     '--ExtraClausesMode',
      'None:END', //Dec_1:Dec_2:IncByTwoToM_1_1:IncByTwoToM_1_2:EQ:Modulo_1:Modulo_2:END',
//     '--AddModuloMod',
      'Decr:IncByTwoToM_1:END',
//     '--Mode',
      'Factoring:RSAFactoring:Multiplication:Division:END',
//     '--InputNumber',
      'None:END',
//     '--a',
      'None:END',
//     '--b',
      'None:END',
//     '--c',
      'None:END',
//     '--InputSize',
       'None:END',
//     '--OutputFilename'
     'None:END'
    );



