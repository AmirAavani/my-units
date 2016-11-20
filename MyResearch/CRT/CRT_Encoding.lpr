program CRT_Encoding;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, gvector, ParameterManagerUnit, GenericCollectionUnit,
  SysUtils, CRTUnit
  { you can add units after this };

procedure Initialize;
begin
  ParameterManagerUnit.Initialize;

end;

procedure Finalize;
begin

  ParameterManagerUnit.Initialize;
end;

function CreateProblemInstance: TCRTProblem;
  function ToIntList(sList: TStringList): TIntList;
  var
    i: Integer;
  begin
    Result := TIntList.Create;
    for i := 0 to SList.Count - 1 do
      Result.Add(StrToInt(sList[i]));

  end;

var
  Ais, Mis: TIntList;
  SList: TStringList;

begin
  SList := TStringList.Create;
  SList.Delimiter := ',';
  sList.StrictDelimiter := True;
  SList.DelimitedText := GetRunTimeParameterManager.ValueByName['Ai'];

  Ais := ToIntList(SList);

  SList.Clear;
  SList.Delimiter := ',';
  sList.StrictDelimiter := True;
  SList.DelimitedText := GetRunTimeParameterManager.ValueByName['Mi'];
  Mis := ToIntList(SList);

  SList.Free;

  Result := TCRTProblem.Create(Mis, Ais);
  Ais.Free;
  Mis.Free;

end;

var
  CRTProblem: TCRTProblem;

begin
  Initialize;

  CRTProblem := CreateProblemInstance;
  WriteLn(CRTProblem.ToString);

  Finalize;
end.

