unit ElfHashUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ElfHash(constref Value: AnsiString): UInt32;
function ElfHash(Start: PChar; Length: Integer): UInt32;
function ElfHash(Start, Last: PChar): UInt32;

implementation

function ElfHash(constref Value: AnsiString): UInt32;
var
  pc: PChar;

begin
  pc := @(Value[1]);
  Result := ElfHash(pc, pc + Length(Value));

end;

function ElfHash(Start: PChar; Length: Integer): UInt32;
begin
  Result := ElfHash(Start, Start+Length);
end;

function ElfHash(Start, Last: PChar): UInt32;
var
  pc: PChar;
  x: UInt32;

begin
  Result := 0;
  pc := Start;

  while pc <= Last do
  begin
    Result := (Result shl 4) + Ord(pc^);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);

    Inc(pc);
  end;
end;

end.

