unit StringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes;

function IsPrefix(const Prefix, Str: AnsiString): Boolean;
function IsSuffix(const Suffix, Str: AnsiString): Boolean;
function Split(const Str: AnsiString; Delimiter: Char): TStringList;
function JoinStrings(const Strings: TStringList; Separator: AnsiString): AnsiString;

implementation
uses
  sysutils, strings;

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

function Split(const Str: AnsiString; Delimiter: Char): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := Str;

end;

function JoinStrings(const Strings: TStringList; Separator: AnsiString
  ): AnsiString;
var
  Str: AnsiString;
  i: Integer;

begin
  Result := '';
  for i := 0 to Strings.Count - 1 do
  begin
    Str := Strings[i];

    if i <> 0 then
      Result += Separator;
    Result += Str;
  end;
end;

end.

