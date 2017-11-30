program CRT_Encoding;

{$mode objfpc}{$H+}
{$Assertions on}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, gvector, ParameterManagerUnit, WideStringUnit, SysUtils,
  BaseEncoderUnit, BaseConstraintUnit, ClauseUnit, SatSolverInterfaceUnit,
  BinaryArithmeticCircuitUnit, BaseArithmeticCircuitUnit, BaseCircuitUnit,
  MiniSatSolverInterfaceUnit, CRTEncoderUnit, StreamUnit, CRTConstraintUnit,
  NumberTheoryUnit, BitVectorUnit, TSeitinVariableUnit, BigInt;//, heaptrc;

function CreateProblemInstance: TCRTConstraint;
  function ToIntList(sList: TStringList): TIntList;
  var
    i: Integer;
  begin
    Result := TIntList.Create;
    for i := 0 to SList.Count - 1 do
      Result.Add(StrToInt(sList[i]));

  end;

var
  i, j: Integer;
  Ais, Mis: TIntList;
  SList: TStringList;
  Error: Boolean;
  AiBitVectorList : TBitVectorList;
  ai, mi, lcm, tmp: TBigInt;

begin
  if GetRunTimeParameterManager.ValueByName['--ProblemMode'] = 'FindN' then
  begin
    SList := TStringList.Create;
    SList.Delimiter := ',';
    sList.StrictDelimiter := True;
    SList.DelimitedText := GetRunTimeParameterManager.ValueByName['--Ri'];

    Ais := ToIntList(SList);
    AiBitVectorList := TBitVectorList.Create;
    for i := 0 to Ais.Count - 1 do
    begin
      ai := BigIntFactory.GetNewMember;
      ai.SetValue(Ais[i]);
      AiBitVectorList.Add(BinaryArithmeticCircuitUnit.GetBinaryArithmeticCircuit.GenerateBinaryRep(ai));
      ai.Release;
    end;

    SList.Clear;
    SList.Delimiter := ',';
    sList.StrictDelimiter := True;
    SList.DelimitedText := GetRunTimeParameterManager.ValueByName['--Mi'];
    Mis := ToIntList(SList);

    lcm := BigIntFactory.GetNewMember.SetValue(1);
    for i := 0 to Mis.Count - 1 do
    begin
      mi := BigIntFactory.GetNewMember.SetValue(Mis[i]);
      tmp := lcm.lcm(mi);

      mi.Release;
      lcm.Release;
      lcm := tmp;
    end;

    SList.Free;

    Error := Mis.Count <> Ais.Count;
    if Error then
    begin
      Ais.Free;
      Mis.Free;
      Exit(nil);
    end;

    Result := TCRTConstraint.Create(
      TBitVector.Create(lcm.Log),
      Mis,
      AiBitVectorList);


    lcm.Release;
    Ais.Free;
  end
  else
    Assert(False, GetRunTimeParameterManager.ValueByName['--ProblemMode']);

end;

var
  CRTProblem: TCRTConstraint;
  CRTEncoder: TBaseEncoder;
  Encoding: TEncoding;
  OutputCNF: TMyTextStream;

begin
  CRTProblem := CreateProblemInstance;
  if CRTProblem = nil then
  begin

    WriteLn('Some Errors in problem specification');
    Halt(1);
  end;
  WriteLn(CRTProblem.ToString);

  CRTEncoder := TBaseCRTEncoder.GetEncoder(GetRunTimeParameterManager.ValueByName['--Encoder']);
  Encoding := CRTEncoder.Encode(CRTProblem);

  OutputCNF := TMyTextStream.Create(TFileStream.Create(GetRunTimeParameterManager.ValueByName['--CNFOutput'], fmCreate));

  WriteLn(Encoding.Output.ToString);
  Encoding.Clauses.Save(OutputCNF);

  OutputCNF.Free;
  CRTEncoder.Free;
  CRTProblem.Free;

end.

