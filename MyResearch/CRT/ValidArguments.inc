ArgumentCount = 7;
ValidArguments: array [1..ArgumentCount] of AnsiString =
 ('--ProblemMode', {Problem Mode}
  '--N', {N : Will be present only if --ProblemMode is FindRs.}
  '--Mi', {A comma separated list of Integers, determining the N modulos.}
  '--Ri', {A comma separated list of Integers, determining the N remainder.}
  '--Encoder', {Name of the CRTEncoder Modulo.}
  '--SatSolverType',
  '--CNFOutput',
  '--Verbose' {The verbosity level}
);


ValidArgumentsValues: array [1..ArgumentCount] of AnsiString =
('FindN:FindRs', 'NONE', 'NONE', 'NONE', 'BasicEncoder', 'CNFCollection', 'NONE', 'NONE');

