unit UTF8Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit;
type
  TRune = UInt64;
  TRuneList = specialize TCollection<TRune>;

  function GetAllRunes(const Text: AnsiString): TRuneList;
  procedure GetAllRunes(const Text: AnsiString; out Output: TRuneList);
  function GetNextRune(var PText: PChar; Width: PInteger = nil): TRune;
  function RuneToUTF8(aRune: TRune; var Data: array of Byte): Integer;
  function Append(constref Text: AnsiString; aRune: TRune): AnsiString;

implementation

function GetAllRunes(const Text: AnsiString): TRuneList;
begin
  Result := TRuneList.Create;
  GetAllRunes(Text, Result);
end;

procedure GetAllRunes(const Text: AnsiString; out Output: TRuneList);
var
  Pos, Width: Integer;
  PText: PChar;

begin
  PText:= PChar(Text);
  Pos := 1;
  while Pos <= Length(Text) do
  begin
    Output.Add(GetNextRune(PText, @Width));
    Inc(Pos, Width);
  end;
end;

function GetNextRune(var PText: PChar; Width: PInteger): TRune;
var
  b1, b2, b3, b4, w: Byte;

begin
  b1 := Ord(PText^);
  Inc(PText);

  if b1 and 128 = 0 then
  begin
    w := 1;
    Result := b1
  end
  else if b1 and 32 = 0 then
  begin
    b2 := Ord(PText^);
    Inc(PText);
    b2 := b2 xor 128;
    b1 := b1 xor (128+ 64);
    Result := b2+ b1 shl 6;
    w := 2;
  end
  else if b1 and 16 = 0 then
  begin
    b2 := Ord(PText^);
    Inc(PText);
    b3:= Ord(PText^);
    Inc(PText);
    b3:= b3 xor 128;
    b2:= b2 xor 128;
    b1:= b1 xor (128+ 64+ 32);
    Result := b3 + b2 shl 6 + b1 shl 12;
    w := 3;
  end
  else if b1 and 8 = 0 then
  begin
    b2 := Ord(PText^);
    Inc(PText);
    b3 := Ord(PText^);
    Inc(PText);
    b4 := Ord(PText^);
    Inc(PText);

    b4:= b4 xor 128;
    b3:= b3 xor 128;
    b2:= b2 xor 128;
    b1:= b1 xor (128+ 64+ 32+ 16);
    Result := b4+ b3 shl 6+ b2 shl 12+ (b1 shl 18);
    w := 4;
  end
  else
    raise Exception.Create('Invalid Char' + PText);
  if Width <> nil then
    Width^ := w;
end;

function RuneToUTF8(aRune: TRune; var Data: array of Byte): Integer;
begin
  if aRune <= $7F then
  begin
    data[0] := aRune;
    Exit(1);

  end;
  if aRune <= $7FF then
  begin
    Data[0] := (aRune shr 6) or $C0;
    Data[1] := (aRune and $3F) or $80;
    Exit(2);

  end;
  if aRune <= $10000 then
  begin
    Data[0] := (aRune shr 12) or $E0;
    Data[1] := ((aRune shr 6) and $3F) or $80;
    Data[2] := (aRune and $3F) or $80;
    Exit(3);

  end;
  if aRune < $110000 then
  begin
    Data[0] := (aRune shr 18) or $E0;
    Data[1] := ((aRune shr 12) and $3F) or $80;
    Data[2] := ((aRune shr 6) and $3F) or $80;
    Data[3] := (aRune and $3F) or $80;
    Exit(4);

  end;

  Result := 0;

end;

function Append(constref Text: AnsiString; aRune: TRune): AnsiString;
var
  Data: array [0..3] of Byte;
  Width: Integer;
  i: Integer;

begin
  Width := RuneToUTF8(aRune, Data);

  Result := Text;
  for i := 0 to Width - 1 do
    Result += Chr(Data[i]);

end;

end.

