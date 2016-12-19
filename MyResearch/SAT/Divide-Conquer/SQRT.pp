program SQRT;
uses BinaryArithmeticCircuitUnit, BitVectorUnit, ClauseUnit,
  TSeitinVariableUnit, ParameterManagerUnit, SatSolverInterfaceUnit,
  MiniSatSolverInterfaceUnit, GenericStackUnit, WideStringUnit, BigInt, contnrs,
  Math, fgl, BaseLogicCircuits;

procedure Initialize;
begin
  ParameterManagerUnit.Initialize;
  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
end;

procedure Finalize;
begin
  SatSolverInterfaceUnit.Finalize;
  ParameterManagerUnit.Finalize;
  TSeitinVariableUnit.Finalize;
end;

function NaiveSqrtEncoding(n: TBigInt): TEncoding;
var
  nVector: TBitVector;
  v, w: TBitVector;
  v2, w2: TBitVector;
  Lit: TLiteral;
  Lits: TLiteralCollection;
  Circuit: TBinaryArithmeticCircuit;

begin

  Circuit := BinaryArithmeticCircuitUnit.TBinaryArithmeticCircuit.Create;

  WriteLn(n.ToString);
  nVector := Circuit.GenerateBinaryRep(n);
  WriteLn(nVector.ToString);

  v := TBitVector.Create(1 + n.Log div 2);
  w := TBitVector.Create(v.Count + 1);
  WriteLn('v = ' + v.ToString);
  WriteLn('w = ' + w.ToString);
  GetSatSolver.AddComment('v = ' + v.ToString);
  GetSatSolver.AddComment('w = ' + w.ToString);

  Lits := TLiteralCollection.Create;
  v2 := TBitVector.Create(2 * v.Count);
  Lits.Add(Circuit.EncodeMul(v, v, v2));
  Lits.Add(Circuit.EncodeIsLessThanOrEq(v2, nVector));

  w2 := TBitVector.Create(2 * w.Count);
  Lits.Add(Circuit.EncodeIncr(v, w));

  Lits.Add(Circuit.EncodeMul(w, w, w2));
  Lits.Add(Circuit.EncodeIsLessThan(nVector, w2));

  Lit := GetVariableManager.CreateVariableDescribingAND(Lits);

  Circuit.Free;
  Lits.Free;
  v.Free; w.Free;
  v2.Free; w2.Free;

  Result := TEncoding.Create(Lit, V);
end;

type
  TBitVectorList = specialize TFPGList<TBitVector>;

function BinSearchSqrtEncoding(n: TBigInt): TEncoding;
var
  nVector: TBitVector;
  Tops, Bots, Mids: TBitVectorList;
  Mid2s: TBitVectorList;
  MLE, MG: TBitVector;
  i, m: Integer;
  Lit: TLiteral;
  Lits: TLiteralCollection;
  Circuit: TBinaryArithmeticCircuit;

begin
  Circuit := BinaryArithmeticCircuitUnit.TBinaryArithmeticCircuit.Create;
  Lits := TLiteralCollection.Create;

  WriteLn('n = ', n.ToString);
  nVector := Circuit.GenerateBinaryRep(n);
  WriteLn('nVector = ', nVector.ToString);

  m := 1 + n.Log div 2;
  Tops := TBitVectorList.Create; Bots := TBitVectorList.Create;
  Mids := TBitVectorList.Create; Mid2s := TBitVectorList.Create;

  Bots.Add(TBitVector.Create(m, GetVariableManager.FalseLiteral));
  Tops.Add(TBitVector.Create(m, GetVariableManager.TrueLiteral));
  Mids.add(TBitVector.Create(m)); Mid2s.add(TBitVector.Create(2 * m));

  GetSatSolver.AddComment(Mids[0].ToString);
  GetSatSolver.AddComment(Mid2s[0].ToString);
  Lits.Add(Circuit.EncodeAvg(Bots[0], Tops[0], Mids[0]));
  Lits.Add(Circuit.EncodeMul(Mids[0], Mids[0], Mid2s[0]));

  MLE := TBitVector.Create(m + 1, GetVariableManager.FalseLiteral);
  MG := TBitVector.Create(m + 1, GetVariableManager.FalseLiteral);

  MLE[0] := Circuit.EncodeIsLessThanOrEq(Mid2s[0], nVector);
  MG[0] := NegateLiteral(MLE[0]);
  WriteLn('m[0]= ', Mids[0].ToString, ' m[0]^2=', Mid2s[0].ToString, ' LE=',
    LiteralToString(MLE[0]), ' vs G=', LiteralToString(MG[0]));

  for i := 1 to m - 1 do
  begin
    Circuit.EncodeIFE();
  end;
  Result := TEncoding.Create(GetVariableManager.CreateVariableDescribingAND(Lits),
    Mids[0]);
  Circuit.Free;
  nVector.Free;
end;
var
  n: TBigInt;
  Encoding: TEncoding;

begin
  Initialize;

  n := BigIntFactory.GetNewMember;
  n.LoadFromString(@GetRunTimeParameterManager.ValueByName['--n'][1]);
  if upcase(GetRunTimeParameterManager.ValueByName['--Mode']) = upcase('Naive') then
    Encoding := NaiveSqrtEncoding(n)
  else if upcase(GetRunTimeParameterManager.ValueByName['--Mode']) = upcase('BinSearch') then
    Encoding := BinSearchSqrtEncoding(n)
  else
  begin
    WriteLn('Invalid Mode = ', GetRunTimeParameterManager.ValueByName['--Mode']);
    halt(1);
  end;

  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(Encoding.Lit);
  GetSatSolver.SubmitClause;
  GetSatSolver.Solve;
  Encoding.Free;


  Finalize;
end.
