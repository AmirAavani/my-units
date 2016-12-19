  ArgumentCount = 5;
  ValidArguments : array [1..ArgumentCount] of String = ('--Verbosity',
  '--OutputFilename', '--n', '--SatSolverType', '--Mode');
  ValidArgumentsValues: array [1..ArgumentCount] of String =
 ('NONE', 'NONE', 'NONE', 'CNFCollection', 'Naive:BinSearch');
