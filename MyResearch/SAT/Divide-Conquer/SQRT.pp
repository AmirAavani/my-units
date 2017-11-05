program SQRT;
uses
  SysUtils, BinaryArithmeticCircuitUnit, BitVectorUnit, ClauseUnit,
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

function NaiveSqrtEncoding(n: TBigInt): TBitVector;
var
  nVector: TBitVector;
  v, w: TBitVector;
  v2, w2: TBitVector;
  Circuit: TBinaryArithmeticCircuit;

begin
  Circuit := BinaryArithmeticCircuitUnit.TBinaryArithmeticCircuit.Create;

  WriteLn(n.ToString);
  nVector := Circuit.GenerateBinaryRep(n);
  WriteLn(nVector.ToString);
  v := TBitVector.Create(1 + n.Log div 2);
  Result := v;
  WriteLn('v = ' + v.ToString);
  GetSatSolver.AddComment('v = ' + v.ToString);

  v2 := Circuit.EncodeMul(v, v);
  WriteLn('v2 = ' + v2.ToString);
  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(Circuit.EncodeIsLessThanOrEq(v2, nVector));
  GetSatSolver.SubmitClause;

  w := Circuit.EncodeIncr(v);
  WriteLn('w = ' + w.ToString);
  GetSatSolver.AddComment('w = ' + w.ToString);

  w2 := Circuit.EncodeMul(w, w);
  WriteLn('w2 = ' + w2.ToString);
  GetSatSolver.AddComment('w2 = ' + w2.ToString);

  //Circuit.EncodeIsLessThan(nVector, w2);
  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(Circuit.EncodeIsLessThan(nVector, w2));
  GetSatSolver.SubmitClause;
  Circuit.Free;

  w.Free;
  v2.Free; w2.Free;
  Result := v;
end;

type

  { TBitVectorList }

  TBitVectorList = class(specialize TFPGList<TBitVector>)
    destructor Destroy; override;
  end;

function BinSearchSqrtEncoding(n: TBigInt): TBitVector;
(*
while (low != high) {
    int mid = (low + high) / 2; // Or a fancy way to avoid int overflow
    if (arr[mid] < target) {
        /* This index, and everything below it, must not be the first element
         * greater than or eq what we're looking for because this element is no greater
         * than the element.
         */
        low = mid + 1;
    }
    else {
        /* This element is at least as large as the element, so anything after it can't
         * be the first element that's at least as large.
         */
        high = mid;
    }
}
*)

var
  Target: TBitVector;
  Tops, Bots, Mids,
  FMids: TBitVectorList;
  Nos: TBitVector;
  i, m: Integer;
  ArithCircuit: TBinaryArithmeticCircuit;
  LogicCircut: TBaseLogicCircuit;
  Tmp: TBitVector;

begin
  {
  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(GetVariableManager.TrueLiteral);
  GetSatSolver.AddLiteral(GetVariableManager.TrueLiteral);
  GetSatSolver.SubmitAndGate(GetVariableManager.TrueLiteral);
  Exit(nil);
}
  ArithCircuit := BinaryArithmeticCircuitUnit.TBinaryArithmeticCircuit.Create;
  LogicCircut := TBaseLogicCircuit.Create;

  // 2^{n.Log} - 1 <= 2^{(n.Log + 1) /2}
  m := (1 + n.Log) div 2 + 1;
  Tops := TBitVectorList.Create; Bots := TBitVectorList.Create;
  Mids := TBitVectorList.Create; FMids := TBitVectorList.Create;

  Bots.Add(TBitVector.Create(m, GetVariableManager.FalseLiteral));
  Tops.Add(TBitVector.Create(m, GetVariableManager.TrueLiteral));

  for i := 0 to m do
  begin
    if i <> 0 then
    begin
      Bots.Add(TBitVector.Create(m));
      Tops.Add(TBitVector.Create(m));
    end;
    Mids.Add(ArithCircuit.Avg(Tops[i], Bots[i]));
    FMids.Add(ArithCircuit.Mul(Mids[i], Mids[i]));
  end;
  GetSatSolver.AddComment('**');

  WriteLn('n = ', n.ToString);
  Target := ArithCircuit.GenerateBinaryRep(n);
  WriteLn('Target = ', Target.ToString);

  for i := 0 to m do
  begin
    WriteLn('Tops[', i, '] = ', Tops[i].ToString);
    WriteLn('Bots[', i, '] = ', Bots[i].ToString);
    WriteLn('Mids[', i, '] = ', Mids[i].ToString);
    WriteLn('FMids[', i, '] = ', FMids[i].ToString);
  end;

  Nos := TBitVector.Create(m, GetVariableManager.FalseLiteral);

  for i := 0 to m - 1 do
  begin
    WriteLn(FMids[i].ToString, ' ', Target.ToString);
    Nos[i] := ArithCircuit.EncodeIsLessThanOrEq(FMids[i], Target);
    WriteLn('m[', i, ']= ', Mids[i].ToString, ' m[i]^2=', FMids[i].ToString, ' LS=',
      LiteralToString(Nos[i]));

    // Bots[i + 1] = MidPlusOnes[i] if Ls[i];
    // Bots[i + 1] = Bots[i] ow;
    Tmp := LogicCircut.EncodeITE(Nos[i], Mids[i], Bots[i]);
    ArithCircuit.SubmitIsEqual(Tmp, Bots[i + 1], GetVariableManager.TrueLiteral);
    Tmp.Free;

    // Tops[i + 1] = Top[i] if Ls[i];
    // Tops[i + 1] = Mids[i] ow;
    Tmp := LogicCircut.EncodeITE(Nos[i], Tops[i], Mids[i]);
    ArithCircuit.SubmitIsEqual(Tmp, Tops[i + 1], GetVariableManager.TrueLiteral);
    Tmp.Free;
  end;

  Result := Bots[m].Copy;
  WriteLn('Bot, Top[m] "', Bots[m].ToString, '" "', Tops[m].ToString, '"');
  WriteLn('Bot, Top[m-1] "', Bots[m - 1].ToString, '" "', Tops[m - 1].ToString, '"');
  WriteLn('REsult = "', Result.ToString, '"');

  Nos.Free;
  FMids.Free;
  Mids.Free;
  Bots.Free;
  Tops.Free;

  ArithCircuit.Free;
  Target.Free;
end;

var
  n: TBigInt;
  Encoding: TBitVector;

{ TBitVectorList }

destructor TBitVectorList.Destroy;
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    Items[i].Free;

  inherited Destroy;
end;

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

  // WriteLn(Encoding.ToString);
  GetSatSolver.Solve;
  Encoding.Free;

  Finalize;
end.
