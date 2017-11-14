ArgumentCount = 7;
ValidArguments: array [1..ArgumentCount] of AnsiString =
 ('--N', {Number of Modulos.}
  '--Mi', {A comma separated list of Integers, determining the N modulos.}
  '--Ai', {A comma separated list of Integers, determining the N remainder.}
  '--Encoder', {Name of the CRTEncoder Modulo.}
  '--SatSolverType',
  '--CNFOutput',
  '--Verbose' {The verbosity level}
);


ValidArgumentsValues: array [1..ArgumentCount] of AnsiString =
('NONE', 'NONE', 'NONE', 'BasicEncoder', 'CNFCollection', 'NONE', 'NONE');

