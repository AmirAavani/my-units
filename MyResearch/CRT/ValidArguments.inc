ArgumentCount = 9;
ValidArguments: array [1..ArgumentCount] of AnsiString =
 ('--ProblemMode', {Problem Mode}
  '--N', {N : Will be present only if --ProblemMode is FindRs.}
  '--Mi', {A comma separated list of Integers, determining the N modulos.}
  '--Ri', {A comma separated list of Integers, determining the N remainder.}
  '--NBit', {Nbit : Number of bits in N. It will be present only if --ProblemMode is FindN}
  '--Encoder', {Name of the CRTEncoder Modulo.}
  '--SatSolverType',
  '--CNFOutput',
  '--Verbose' {The verbosity level}
);


ValidArgumentsValues: array [1..ArgumentCount] of AnsiString =
('FindN:FindRs', 'NONE', 'NONE', 'NONE', 'None', 'BasicCRTEncoder', 'CNFCollection',
  'NONE', 'NONE');

