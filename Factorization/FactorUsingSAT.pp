program FactorUsingSAT;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SatSolverInterfaceUnit, ParameterManagerUnit, SysUtils, contnrs,
  gvector, BigInt, FactoringUsingSATUnit, AbstractSolverUnit, CNFCollectionUnit,
  MiniSatSolverInterfaceUnit, TSeitinVariableUnit,
  BinaryEncodingBasedFactoringUnit, BitVectorUnit, ClauseUnit,
  BinaryArithmeticCircuitUnit, BaseArithmeticCircuitUnit, BaseCircuitUnit,
  ModuloBasedFactoringUnit, GenericCollectionUnit, StreamUnit,
  GenericFactoryUnit, GenericStackUnit, WideStringUnit;

procedure Initialize;
begin
  ParameterManagerUnit.Initialize;
  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
  FactoringUsingSATUnit.Initialize;//('BinaryRep');

end;

procedure Finalize;
begin
  FactoringUsingSATUnit.Finalize;

  SatSolverInterfaceUnit.Finalize;
  TSeitinVariableUnit.Finalize;
  ParameterManagerUnit.Finalize;

end;

procedure EncodeMultiplication;
var
  na, nb: TBigInt;
  a, b, c: TBitVector;
  arep, brep: TBitVector;
  sa, sb: AnsiString;
  ArithmeticCircuit: TBaseArithmeticCircuit;
  InputSize: Integer;

begin
  assert(
         (((GetRunTimeParameterManager.ValueByName['--a'] <> '') and
         (GetRunTimeParameterManager.ValueByName['--b'] <> '')) or
         (GetRunTimeParameterManager.ValueByName['--InputSize'] <> '')),
         'a = ' + GetRunTimeParameterManager.ValueByName['--a'] + ' b =' +
         GetRunTimeParameterManager.ValueByName['--b'] + ' InputSize = ' +
         GetRunTimeParameterManager.ValueByName['--InputSize']);

  ArithmeticCircuit := nil;

  if GetRunTimeParameterManager.ValueByName['--MultiplierMode'] = 'BinaryRep' then
    ArithmeticCircuit := TBinaryArithmeticCircuit.Create
  else if GetRunTimeParameterManager.ValueByName['--MultiplierMode'] = 'ModuloRep' then
    ArithmeticCircuit := TModuloBasedBinaryArithmeticCircuit.Create;
  assert(ArithmeticCircuit <> nil);

  if (GetRunTimeParameterManager.ValueByName['--a'] <> '') and
         (GetRunTimeParameterManager.ValueByName['--b'] <> '') then
  begin
    sa := GetRunTimeParameterManager.ValueByName['--a'];
    sb := GetRunTimeParameterManager.ValueByName['--b'];

    na := BigIntFactory.GetNewMember.LoadFromString(@sa[1]);
    nb := BigIntFactory.GetNewMember.LoadFromString(@sb[1]);

    WriteLn('na.Log = ', na.Log, 'nb.Log = ', nb.Log);


    arep := ArithmeticCircuit.GenerateBinaryRep(na);
    brep := ArithmeticCircuit.GenerateBinaryRep(nb);

    a := TBitVector.Create(arep.Count);
    b := TBitVector.Create(brep.Count);

    c := TBitVector.Create(a.Count + b.Count);
    WriteLn('c a = ', a.ToString);
    WriteLn('c b = ', b.ToString);
    WriteLn('c n = ', c.ToString);

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeMul(a, b, c, 0));
    SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(arep, a));
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(brep, b));
    SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(GetVariableManager.TrueLiteral);

    BigIntFactory.ReleaseMember(na);
    BigIntFactory.ReleaseMember(nb);
    a.Free;
    b.Free;
    c.Free;

  end
  else if GetRunTimeParameterManager.ValueByName['--InputSize'] <> '' then
  begin
    InputSize := StrToInt(GetRunTimeParameterManager.ValueByName['--InputSize']);

    na := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(InputSize + 1).Decr;
    nb := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(InputSize + 1).Decr;

    WriteLn('na.Log = ', na.Log, 'nb.Log = ', nb.Log);


    arep := ArithmeticCircuit.GenerateBinaryRep(na);
    brep := ArithmeticCircuit.GenerateBinaryRep(nb);

    a := TBitVector.Create(arep.Count);
    b := TBitVector.Create(brep.Count);

    c := TBitVector.Create(a.Count + b.Count);
    WriteLn('c a = ', a.ToString, ' arep = ', arep.ToString);
    WriteLn('c b = ', b.ToString, ' brep = ', brep.ToString);
    WriteLn('c n = ', c.ToString);

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeMul(a, b, c, 0));
    SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(arep, a));
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(brep, b));
    SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(GetVariableManager.TrueLiteral);

    BigIntFactory.ReleaseMember(na);
    BigIntFactory.ReleaseMember(nb);
    a.Free;
    b.Free;
    c.Free;
  end;

  ArithmeticCircuit.Free;

end;


procedure EncodeDivision;
var
  na, nc: TBigInt;
  m, x, h, S, c, Mul: TBitVector;
  mrep, crep: TBitVector;
  sa, sc: AnsiString;
  ArithmeticCircuit: TBaseArithmeticCircuit;
  InputSize: Integer;
  i : Integer;

begin
  Randomize;
  assert(
         (((GetRunTimeParameterManager.ValueByName['--a'] <> '') and
         (GetRunTimeParameterManager.ValueByName['--c'] <> '')) or
         (GetRunTimeParameterManager.ValueByName['--InputSize'] <> '')),
         'a = ' + GetRunTimeParameterManager.ValueByName['--a'] + ' c =' +
         GetRunTimeParameterManager.ValueByName['--c'] + ' InputSize = ' +
         GetRunTimeParameterManager.ValueByName['--InputSize']);

  ArithmeticCircuit := nil;

  if GetRunTimeParameterManager.ValueByName['--MultiplierMode'] = 'BinaryRep' then
    ArithmeticCircuit := TBinaryArithmeticCircuit.Create
  else if GetRunTimeParameterManager.ValueByName['--MultiplierMode'] = 'ModuloRep' then
    ArithmeticCircuit := TModuloBasedBinaryArithmeticCircuit.Create;
  assert(ArithmeticCircuit <> nil);

  if (GetRunTimeParameterManager.ValueByName['--a'] <> '') and
         (GetRunTimeParameterManager.ValueByName['--c'] <> '') then
  begin{
    sa := GetRunTimeParameterManager.ValueByName['--a'];
    sc := GetRunTimeParameterManager.ValueByName['--c'];

    na := BigIntFactory.GetNewMember.LoadFromString(@sa[1]);
    nc := BigIntFactory.GetNewMember.LoadFromString(@sc[1]);

    WriteLn('na.Log = ', na.Log, 'nc.Log = ', nc.Log);

    arep := ArithmeticCircuit.GenerateBinaryRep(na);
    crep := ArithmeticCircuit.GenerateBinaryRep(nc);

    m := TBitVector.Create(arep.Count);
    c := TBitVector.Create(crep.Count);

    WriteLn('c m = ', m.ToString);
    WriteLn('c x = ', x.ToString);
    WriteLn('c n = ', c.ToString);

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeMul(a, b, c, 0));
    SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(arep, a));
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(crep, c));
    SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(GetVariableManager.TrueLiteral);

    BigIntFactory.ReleaseMemeber(na);
    BigIntFactory.ReleaseMemeber(nc);
    a.Free;
    b.Free;
    c.Free;
                                                                             }
  end
  else if GetRunTimeParameterManager.ValueByName['--InputSize'] <> '' then
  begin
    InputSize := StrToInt(GetRunTimeParameterManager.ValueByName['--InputSize']);

    mrep := TBitVector.Create(InputSize, GetVariableManager.TrueLiteral);
    crep := TBitVector.Create(2 * InputSize, GetVariableManager.TrueLiteral);
    for i := 0 to mRep.Count - 2 do
      if Random(10) < 5 then
        mrep[i] := GetVariableManager.FalseLiteral;
    for i := 0 to cRep.Count - 2 do
      if Random(10) < 5 then
        crep[i] := GetVariableManager.FalseLiteral;

    m := TBitVector.Create(mrep.Count);
    c := TBitVector.Create(crep.Count);

    x := TBitVector.Create(InputSize + 1);
    h := TBitVector.Create(m.Count);
    WriteLn('c m = ', m.ToString, ' mrep = ', mrep.ToString);
    WriteLn('c x = ', x.ToString, ' crep = ', crep.ToString);
    WriteLn('c h = ', h.ToString);
    WriteLn('c c = ', c.ToString);

    Mul := TBitVector.Create(m.Count + x.Count);
    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeMul(m, x, Mul, 0));
    SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    SatSolverInterfaceUnit.GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeAdd(Mul, h, c));
    SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

    SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(mrep, m));
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsEqual(crep, c));
    GetSatSolver.AddLiteral(ArithmeticCircuit.EncodeIsLessThan(h, m));
    SatSolverInterfaceUnit.GetSatSolver.SubmitAndGate(GetVariableManager.TrueLiteral);

    m.Free;
    x.Free;
    h.Free;
    c.Free;
    mrep.Free;
    crep.Free;
  end;

  ArithmeticCircuit.Free;

end;

var
  n: TBigInt;
  InputNumber, InputSize: AnsiString;
  a, b: TBitVector;

begin
  if Paramcount= 0 then
  begin
    WriteLn('Invalid Usage!');
    WriteLn(ParamStr(0) + ' n ');
    Halt(1);

  end;

  Initialize;


  if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
             UpperCase('Factoring') then
  begin
    assert((GetRunTimeParameterManager.ValueByName['--InputNumber'] <> '') or
           (GetRunTimeParameterManager.ValueByName['--InputSize'] <> '') );

    InputNumber:= GetRunTimeParameterManager.ValueByName['--InputNumber'];
    InputSize:= GetRunTimeParameterManager.ValueByName['--InputSize'];

    if InputNumber <> '' then
    begin

      n:= BigIntFactory.GetNewMember.LoadFromString(@InputNumber[1]);

      a:= TBitVector.Create(n.Log );
      b:= TBitVector.Create(n.Log);

      WriteLn('c a = ', a.ToString);
      WriteLn('c b = ', b.ToString);
      WriteLn('c n = ', InputNumber);

      SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
      SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, n));
      SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

      BigIntFactory.ReleaseMember(n);

      a.Free;
      b.Free;

    end
    else
    begin
      n := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(StrToInt(InputSize)).Decr;

      a:= TBitVector.Create(n.Log );
      b:= TBitVector.Create(n.Log);

      WriteLn('c a = ', a.ToString);
      WriteLn('c b = ', b.ToString);

      SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
      SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, n));
      SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

      BigIntFactory.ReleaseMember(n);

      a.Free;
      b.Free;

      BigIntFactory.ReleaseMember(n);

    end;
  end else if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
             UpperCase('RSAFactoring') then
  begin
    assert((GetRunTimeParameterManager.ValueByName['--InputNumber'] <> '') or
           (GetRunTimeParameterManager.ValueByName['--InputSize'] <> '') );

    InputNumber:= GetRunTimeParameterManager.ValueByName['--InputNumber'];
    InputSize:= GetRunTimeParameterManager.ValueByName['--InputSize'];

    if InputNumber <> '' then
    begin

      n:= BigIntFactory.GetNewMember.LoadFromString(@InputNumber[1]);

      WriteLn('n.Log = ', n.Log);
//      assert(n.Log  mod 2 = 0);
      a:= TBitVector.Create((n.Log + 3) div 2);
      b:= TBitVector.Create((n.Log + 3) div 2);

      WriteLn('c a = ', a.ToString);
      WriteLn('c b = ', b.ToString);
      WriteLn('c n = ', InputNumber);

      SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
      SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, n));
      SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

      BigIntFactory.ReleaseMember(n);

      a.Free;
      b.Free;

    end
    else
    begin
      n := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(StrToInt(InputSize)).Decr;

      assert(n.Log  mod 2 = 0);
      a:= TBitVector.Create(n.Log div 2);
      b:= TBitVector.Create(n.Log div 2);

      WriteLn('c a = ', a.ToString);
      WriteLn('c b = ', b.ToString);

      SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
      SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, n));
      SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

      BigIntFactory.ReleaseMember(n);

      a.Free;
      b.Free;

      BigIntFactory.ReleaseMember(n);

    end
  end else if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
             UpperCase('Multiplication') then
      EncodeMultiplication
  else if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
           UpperCase('Division') then
    EncodeDivision;

  Finalize;
end.
