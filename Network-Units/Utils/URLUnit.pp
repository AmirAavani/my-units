unit URLUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetDomain(const URL: AnsiString): AnsiString;
function GetProtocol(const URL: AnsiString): AnsiString;

implementation

function GetDomain(const URL: AnsiString): AnsiString;
var
  i: Integer;

begin
  Result := '';
  for i := Length(GetProtocol(URL)) + 4 to Length(URL) do
  begin
    if URL[i] = '/' then
      break;
    Result += URL[i];
  end;
end;

function GetProtocol(const URL: AnsiString): AnsiString;
var
  i: Integer;

begin
  Result := '';

  for i := 1 to Length(URL) do
    if URL[i] = ':' then
    begin
      Result := Copy(URL, 1, i - 1);
      break;
    end;

end;

end.

