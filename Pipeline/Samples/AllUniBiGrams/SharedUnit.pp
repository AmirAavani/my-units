unit SharedUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  PageTag = '<page>';
  EndPageTag = '</page>';

function GetPositionFileName(TaskID, NumTasks: Integer): AnsiString;
function GetExtractFileName(TaskID, NumTasks: Integer): AnsiString;

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

function GetExtractFileName(TaskID, NumTasks: Integer): AnsiString;
begin
  Result := JoinPath(
    GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
    Format('WikiPageFile-%.5d-of-%.5d.bin', [TaskID, NumTasks])
  );

end;


end.

