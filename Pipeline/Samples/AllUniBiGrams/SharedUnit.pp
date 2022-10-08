unit SharedUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  PageTag = '<page>';
  EndPageTag = '</page>';

function GetPositionFileName(TaskID: Integer): AnsiString;
function GetExtractFileName(TaskID: Integer): AnsiString;

implementation
uses
  PathHelperUnit, ParameterManagerUnit;

function GetPositionFileName(TaskID: Integer): AnsiString;
begin
  Result := JoinPath(
    GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
    Format('PositionFile-%d.bin', [TaskID])
  );

end;

function GetExtractFileName(TaskID: Integer): AnsiString;
begin
  Result := JoinPath(
    GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
    Format('WikiPageFile-%d.bin', [TaskID])
  );

end;


end.

