program InstanceGenerator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, ParameterManagerUnit, StreamUnit, CNFCollectionUnit,
  TSeitinVariableUnit, SatSolverInterfaceUnit, MiniSatSolverInterfaceUnit,
  BitVectorUnit
  { you can add units after this };

procedure Initalize;
begin
  ParameterManagerUnit.Initialize;
  SatSolverInterfaceUnit.Initialize;
  TSeitinVariableUnit.Initialize;
end;

procedure Finalize;
begin
  SatSolverInterfaceUnit.Finalize;
  TSeitinVariableUnit.Finalize;
  ParameterManagerUnit.Finalize;
end;

procedure GenerateRandomInstance(Index, m, n: Integer);
  function GetIndex(Index, Size: Integer): AnsiString;
  var
    i: Integer;
  begin
    Result := IntToStr(Index) + '-' + IntToStr(Size);

    for i := Length(IntToStr(Index)) + 1 to Length(IntToStr(Size)) do
      Result := '0' + Result;

  end;

var
  i: Integer;
  BitVector: TBitVector;
  CNFCollection: TCNFCollection;
  OutputFile: TMyTextStream;

begin
  OutputFile := TMyTextStream.Create(TFileStream.Create('Input-'+
    GetIndex(Index, m) + '.cnf', fmCreate), True);
  CNFCollection := SatSolverInterfaceUnit.ReNewSatSolver(GetRunTimeParameterManager.
    ValueByName['--SatSolverType']) as TCNFCollection;
  TSeitinVariableUnit.ReNewVariableManager;

  BitVector := TBitVector.Create(n);

  for i := 1 to n do
  begin
    BitVector[i - 1] := Random(2);
  end;
  WriteLn(BitVector.ToString);

  BitVector.Free;
  CNFCollection.SaveToFile(OutputFile);
  OutputFile.Free;

  TSeitinVariableUnit.PopVariableManager;
  SatSolverInterfaceUnit.PopBackSatSolver;
end;

var
  n, m, Seed: Integer;
  i: Integer;

begin
  Initalize;

  n := StrToInt(GetRunTimeParameterManager.ValueByName['--BitCount']);
  m := StrToInt(GetRunTimeParameterManager.ValueByName['--InstanceCount']);
  Seed := StrToInt(GetRunTimeParameterManager.ValueByName['--RandomSeed']);
  RandSeed := Seed;
  WriteLn(n, ' ', m, ' ', Seed);

  for i := 1 to m do
  begin
    GenerateRandomInstance(i, m, n);
  end;
  Finalize;
end.

