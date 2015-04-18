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
  GenericFactoryUnit, GenericStackUnit;

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
  la, lb: TLiteral;
  sa, sb: AnsiString;
  ArithmeticCircuit: TModuloBasedBinaryArithmeticCircuit;

begin
  sa := GetRunTimeParameterManager.ValueByName['a'];
  sb := GetRunTimeParameterManager.ValueByName['b'];

  na := BigIntFactory.GetNewMember.LoadFromString(@sa[1]);
  nb := BigIntFactory.GetNewMember.LoadFromString(@sb[1]);

  WriteLn('na.Log = ', na.Log, 'nb.Log = ', nb.Log);

  ArithmeticCircuit := TModuloBasedBinaryArithmeticCircuit.Create;

//      assert(n.Log  mod 2 = 0);
  a:= TBitVector.Create(na.Log + 1);
  la := ArithmeticCircuit.EncodeBinaryRep(na, a);
  lb := ArithmeticCircuit.EncodeBinaryRep(nb, b);

  GetSatSolver.BeginConstraint;
  GetSatSolver.AddLiteral(la);
  GetSatSolver.AddLiteral(lb);
  GetSatSolver.SubmitAndGate(GetVariableManager.TrueLiteral);

  c := TBitVector.Create(a.Count + b.Count);
  WriteLn('c a = ', a.ToString);
  WriteLn('c b = ', b.ToString);
  WriteLn('c n = ', c.ToString);

  SatSolverInterfaceUnit.GetSatSolver.BeginConstraint;
  SatSolverInterfaceUnit.GetSatSolver.AddLiteral(FactoringUsingSATUnit.GetActiveFactorizer.GenerateCNF(a, b, c));
  SatSolverInterfaceUnit.GetSatSolver.SubmitClause;

  BigIntFactory.ReleaseMemeber(na);
  BigIntFactory.ReleaseMemeber(nb);
  a.Free;
  b.Free;
  c.Free;

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

  assert((GetRunTimeParameterManager.ValueByName['--InputNumber'] <> '') or
         (GetRunTimeParameterManager.ValueByName['--InputSize'] <> '') );

  InputNumber:= GetRunTimeParameterManager.ValueByName['--InputNumber'];
  InputSize:= GetRunTimeParameterManager.ValueByName['--InputSize'];

  if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
             UpperCase('Factoring') then
  begin
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

      BigIntFactory.ReleaseMemeber(n);

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

      BigIntFactory.ReleaseMemeber(n);

      a.Free;
      b.Free;

      BigIntFactory.ReleaseMemeber(n);

    end;
  end else if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
             UpperCase('RSAFactoring') then
  begin
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

      BigIntFactory.ReleaseMemeber(n);

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

      BigIntFactory.ReleaseMemeber(n);

      a.Free;
      b.Free;

      BigIntFactory.ReleaseMemeber(n);

    end
  end else if UpperCase(GetRunTimeParameterManager.ValueByName['--Mode']) =
             UpperCase('Multiplication') then
      EncodeMultiplication;

  Finalize;
end.
