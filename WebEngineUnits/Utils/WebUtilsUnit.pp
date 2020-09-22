unit WebUtilsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function NormalizeGetString(const Value: AnsiString): AnsiString;
function NormalizePostString(const Value: AnsiString): AnsiString;

implementation

function Decode(const Str: AnsiString): Integer;
var
  Ch: Char;

begin
  Result := 0;

  for Ch in Str do
  begin
    Result *= 16;

    if Ch in ['0'..'9'] then
      Result += Ord(Ch) - 48
    else
      Result += Ord(Ch) - 55;

  end;
end;

function NormalizeGetString(const Value: AnsiString): AnsiString;
var
  Ch: Char;
  i: Integer;

begin
  Result := '';

  i := 1;
  while i <= Length(Value) do
  begin
    Ch := Value[i];
    if Ch = '+' then
      Ch := ' '
    else if Ch = '%' then
    begin
      Result += Chr(Decode(Copy(Value, i + 1, 2)));
      i += 3;
      Continue;
    end;

    Result += Ch;

    Inc(i);
  end;

end;

function NormalizePostString(const Value: AnsiString): AnsiString;
var
  Ch: Char;
  i: Integer;

begin
  Result := '';

  i := 1;
  while i <= Length(Value) do
  begin
    Ch := Value[i];

    if Ch = '%' then
    begin
      Result += Chr(Decode(Copy(Value, i + 1, 2)));
      i += 3;
      Continue;
    end;

    Result += Ch;

    Inc(i);
  end;

end;

end.

