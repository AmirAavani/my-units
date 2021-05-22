unit PCharUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCharSet = set of char;

function Pos(Sub, Main: PChar): Integer;
function Copy(Main: PChar; Len: Integer): AnsiString;
function TrimLeft(Main: PChar; aSetToSkip: TCharSet = []): PChar;
function IsPrefix(Prefix, Main: PChar): Boolean;
function IsSuffix(Suffix, Main: PChar): Boolean;

implementation

function Pos(Sub, Main: PChar): Integer;
var
  i, j: PChar;

begin
  i := Main;
  while i^ <> #0 do
  begin
    j := Sub;

    while j^ <> #0 do
    begin
      if i^ <> j^ then
        Break;
      Inc(j);

    end;
    if j^ = #0 then
      Exit(i - Main);

    Inc(i);
  end;

  Result := -1;

end;

function Copy(Main: PChar; Len: Integer): AnsiString;
begin
  Result := '';

  while (0 < Len) and (Main^ <> #0) do
  begin
    Result += Main^;
    Inc(Main);
    Dec(Len);

  end;

end;

function TrimLeft(Main: PChar; aSetToSkip: TCharSet): PChar;
begin
  Result := Main;

  while Result^ <> #0 do
  begin
    if not (Result^ in aSetToSkip) then
      Exit(Result);

    Inc(Result);

  end;

end;

function IsPrefix(Prefix, Main: PChar): Boolean;
begin
  while (Prefix^ <> #0) and (Main^ <> #0) do
  begin
    if Prefix^ <> Main^ then
      Exit(False);

    Inc(Main);
    Inc(Prefix);
  end;

  Result := Prefix^ = #0;

end;

function IsSuffix(Suffix, Main: PChar): Boolean;
var
  Last: PChar;

begin
  Last := Main;
  while Last^ <> #0 do
    Inc(Last);
  while (Suffix^ <> #0) and (Last <> Main) do
  begin
    if Suffix^ <> Last^ then
      Exit(False);

    Dec(Last);
    Inc(Suffix);
  end;

  Result := Suffix^ = #0;


end;

end.

