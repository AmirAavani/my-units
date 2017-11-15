program CRT_Encoding;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, gvector, ParameterManagerUnit, WideStringUnit, SysUtils,
  BaseEncoderUnit, BaseConstraintUnit, ClauseUnit, SatSolverInterfaceUnit,
  MiniSatSolverInterfaceUnit, CRTEncoderUnit, StreamUnit, CRTConstraintUnit,
  NumberTheoryUnit
  { you can add units after this };

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

begin
  SList := TStringList.Create;
  SList.Delimiter := ',';
  sList.StrictDelimiter := True;
  SList.DelimitedText := GetRunTimeParameterManager.ValueByName['--Ai'];

  Ais := ToIntList(SList);

  SList.Clear;
  SList.Delimiter := ',';
  sList.StrictDelimiter := True;
  SList.DelimitedText := GetRunTimeParameterManager.ValueByName['--Mi'];
  Mis := ToIntList(SList);

  SList.Free;

  Error := False;
  for i := 0 to Mis.Count - 1 do
    for j := i + 1 to Mis.Count - 1 do
      if gcd(Mis[i], Mis[j]) <> 1 then
      begin
        Error := True;
        Break;
      end;

  if Error then
  begin
    Ais.Free;
    Mis.Free;
    Exit(nil);
  end;

  Result := TCRTConstraint.Create(nil, Mis, nil);

  Ais.Free;
  Mis.Free;

end;

var
  CRTProblem: TCRTConstraint;
  CRTEncoder: TBaseEncoder;
  Encoding: TEncoding;
  OutputCNF: TMyTextStream;

begin
  Initialize;

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

  Finalize;
end.

