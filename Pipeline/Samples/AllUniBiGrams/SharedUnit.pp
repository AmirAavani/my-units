unit SharedUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  PageTag = '<page>';

function GetPositionFileName(TaskID, NumTasks: Integer): AnsiString;
function GetExtractUnigramsFileName(TaskID, NumTasks: Integer): AnsiString;
function GetExtractBigramsFileName(TaskID, NumTasks: Integer): AnsiString;

implementation
uses
  PathHelperUnit, ParameterManagerUnit;

function GetPositionFileName(TaskID, NumTasks: Integer): AnsiString;
begin
  Result := JoinPath(
    GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
    Format('PositionFile-%.5d-of-%.5d.bin', [TaskID, NumTasks])
  );

end;

function GetExtractUnigramsFileName(TaskID, NumTasks: Integer): AnsiString;
begin
  Result := JoinPath(
    GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
    Format('WikiPageFile-Uni-%.5d-of-%.5d.bin', [TaskID, NumTasks])
  );

end;

function GetExtractBigramsFileName(TaskID, NumTasks: Integer): AnsiString;
begin
  Result := JoinPath(
    GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
    Format('WikiPageFile-Bi-%.5d-of-%.5d.bin', [TaskID, NumTasks])
  );


end;

end.

