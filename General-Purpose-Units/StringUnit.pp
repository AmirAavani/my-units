unit StringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IsPrefix(const Prefix, Str: AnsiString): Boolean;
function IsSuffix(const Suffix, Str: AnsiString): Boolean;

implementation

function IsPrefix(const Prefix, Str: AnsiString): Boolean;
var
  i: Integer;

begin
  if Length(Str) < Length(Prefix) then
    Exit(False);

  Result := False;
  for i := 1 to Length(Prefix) do
    if Prefix[i] <> Str[i] then
      Exit;

  Result := True;

end;

function IsSuffix(const Suffix, Str: AnsiString): Boolean;
var
  i: Integer;
  StrPtr, SuffixPtr: PChar;

begin
  if Length(Str) < Length(Suffix) then
    Exit(False);

  Result := False;
  SuffixPtr := PChar(Suffix) + Length(Suffix);
  StrPtr := PChar(Str) + Length(Str);
  for i := 1 to Length(Suffix) do
  begin
    if SuffixPtr <> StrPtr then
      Exit;

    Dec(SuffixPtr);
    Dec(StrPtr);
  end;

  Result := True;

end;

end.

