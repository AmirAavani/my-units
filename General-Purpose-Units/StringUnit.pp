unit StringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes;

function IsPrefix(constref Prefix, Str: AnsiString): Boolean;
function IsPrefix(constref Prefix, Str: WideString): Boolean;
function IsSuffix(constref Suffix, Str: AnsiString): Boolean;
function IsSuffix(constref Suffix, Str: WideString): Boolean;
function Split(constref Str: AnsiString; Delimiter: AnsiString): TStringList;
function JoinStrings(const Strings: TStringList; constref Separator: AnsiString): AnsiString;
function JoinStrings(const Strings: array of AnsiString; constref Separator: AnsiString;
   SkipEmptyString: Boolean = True): AnsiString;

implementation
uses
  sysutils, StrUtils;

generic function GenericIsPrefix<T>(constref Prefix, Str: T): Boolean;
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

function IsPrefix(constref Prefix, Str: AnsiString): Boolean;
begin
  Result := specialize GenericIsPrefix<AnsiString>(Prefix, Str);

end;

function IsPrefix(constref Prefix, Str: WideString): Boolean;
begin
  Result := specialize GenericIsPrefix<WideString>(Prefix, Str);

end;


generic function GeneralizedIsSuffix<T>(constref Suffix, Str: T): Boolean;
var
  i: Integer;
  StrPtr, SuffixPtr: PChar;

begin
  if Length(Str) < Length(Suffix) then
    Exit(False);

  Result := False;
  SuffixPtr := PChar(Suffix) + Length(Suffix) - 1;
  StrPtr := PChar(Str) + Length(Str) - 1;
  for i := 1 to Length(Suffix) do
  begin
    if SuffixPtr^ <> StrPtr^ then
      Exit;

    Dec(SuffixPtr);
    Dec(StrPtr);
  end;

  Result := True;

end;

function IsSuffix(constref Suffix, Str: AnsiString): Boolean;
begin
  Result := specialize GeneralizedIsSuffix<AnsiString>(Suffix, Str);

end;

function IsSuffix(constref Suffix, Str: WideString): Boolean;
begin
  Result := specialize GeneralizedIsSuffix<WideString>(Suffix, Str);

end;

function Split(constref Str: AnsiString; Delimiter: AnsiString): TStringList;
var
  StrArray: array of AnsiString;
  S: AnsiString;

begin
  StrArray := SplitString(Str, Delimiter);
  Result := TStringList.Create;
  for S in StrArray do
    Result.Add(S);
  SetLength(StrArray, 0);

end;

function JoinStrings(const Strings: TStringList; constref Separator: AnsiString
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

function JoinStrings(const Strings: array of AnsiString; constref
  Separator: AnsiString; SkipEmptyString: Boolean): AnsiString;
var
  Str: AnsiString;
  i: Integer;

begin
  Result := '';

  for i := 0 to High(Strings) do
  begin
    Str := Strings[i];
    if SkipEmptyString and (Length(Str) = 0) then
      Continue;

    if Length(Result) <> 0 then
      Result += Separator;
    Result += Str;
  end;
end;

end.

