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
  w := TBitVector.Create(v.Count + 1);
  WriteLn('v = ' + v.ToString);
  WriteLn('w = ' + w.ToString);
  GetSatSolver.AddComment('v = ' + v.ToString);
  GetSatSolver.AddComment('w = ' + w.ToString);

  v2 := Circuit.EncodeMul(v, v);
  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(Circuit.EncodeIsLessThanOrEq(v2, nVector));
  GetSatSolver.SubmitClause;

  w2 := TBitVector.Create(2 * w.Count);
  w := Circuit.EncodeIncr(v);
  w2 := Circuit.EncodeMul(w, w);

  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(Circuit.EncodeIsLessThan(nVector, w2));
  GetSatSolver.SubmitClause;

  Circuit.Free;

  w.Free;
  v2.Free; w2.Free;

  //Result := TEncoding.Create(Lit, V);
end;

type
  TBitVectorList = specialize TFPGList<TBitVector>;

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
  MidPlusOnes: TBitVectorList;
  FMids: TBitVectorList;
  Ls: TBitVector;
  i, m: Integer;
  ArithCircuit: TBinaryArithmeticCircuit;
  LogicCircut: TBaseLogicCircuit;
  Tmp: TBitVector;

begin
  ArithCircuit := BinaryArithmeticCircuitUnit.TBinaryArithmeticCircuit.Create;
  LogicCircut := TBaseLogicCircuit.Create;

  m := 1 + n.Log div 2;
  Tops := TBitVectorList.Create; Bots := TBitVectorList.Create;
  Mids := TBitVectorList.Create; FMids := TBitVectorList.Create;
  MidPlusOnes := TBitVectorList.Create;

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
    MidPlusOnes.Add(ArithCircuit.Incr(Mids[i]));
    FMids.Add(ArithCircuit.Mul(Mids[i], Mids[i]));

  end;
  GetSatSolver.AddComment('**');

  WriteLn('n = ', n.ToString);
  Target := ArithCircuit.GenerateBinaryRep(n);
  WriteLn('Target = ', Target.ToString);

  // Top[0] = n

  for i := 0 to m do
  begin
    WriteLn('Tops[', i, '] = ', Tops[i].ToString);
    WriteLn('Bots[', i, '] = ', Bots[i].ToString);
    WriteLn('Mids[', i, '] = ', Mids[i].ToString);
    WriteLn('MidPlusOnes[', i, '] = ', MidPlusOnes[i].ToString);
    WriteLn('FMids[', i, '] = ', FMids[i].ToString);
  end;

  Ls := TBitVector.Create(m, GetVariableManager.FalseLiteral);

  for i := 0 to m - 1 do
  begin
    Ls[i] := ArithCircuit.EncodeIsLessThan(FMids[i], Target);
    WriteLn('m[', i, ']= ', Mids[i].ToString, ' m[i]^2=', FMids[i].ToString, ' LS=',
      LiteralToString(LS[i]));

    // Bots[i + 1] = MidPlusOnes[i] if Ls[i];
    // Bots[i + 1] = Bots[i] ow;
    Tmp := LogicCircut.EncodeITE(Ls[i], MidPlusOnes[i], Bots[i]);
    ArithCircuit.SubmitIsEqual(Tmp, Bots[i + 1], GetVariableManager.TrueLiteral);
    Tmp.Free;

    // Tops[i + 1] = Top[i] if Ls[i];
    // Tops[i + 1] = Mids[i] ow;
    Tmp := LogicCircut.EncodeITE(Ls[i], Tops[i], Mids[i]);
    ArithCircuit.SubmitIsEqual(Tmp, Tops[i + 1], GetVariableManager.TrueLiteral);
    Tmp.Free;
  end;

  Result := Mids[m];

  ArithCircuit.Free;
  Target.Free;
end;

var
  n: TBigInt;
  Encoding: TBitVector;

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

  WriteLn(Encoding.ToString);
  GetSatSolver.Solve;
  Encoding.Free;


  Finalize;
end.
