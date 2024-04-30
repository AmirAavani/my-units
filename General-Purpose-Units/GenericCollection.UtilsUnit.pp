unit GenericCollection.UtilsUnit;

{$mode ObjFPC}{$H+}

interface
uses
  classes, TupleUnit;

type
  TBytes = array of Byte;

  function SaveInt8(constref n: Int8; Stream: TStream): Boolean; inline;
  function SaveInt16(constref n: Int16; Stream: TStream): Boolean; inline;
  function SaveInt32(constref n: Int32; Stream: TStream): Boolean; inline;
  function SaveInt64(constref n: Int64; Stream: TStream): Boolean; inline;
  function SaveUInt8(constref n: UInt8; Stream: TStream): Boolean; inline;
  function SaveUInt16(constref n: UInt16; Stream: TStream): Boolean; inline;
  function SaveUInt32(constref n: UInt32; Stream: TStream): Boolean; inline;
  function SaveUInt64(constref n: uInt64; Stream: TStream): Boolean; inline;
  function SaveWideString(constref w: WideString; Stream: TStream): Boolean; inline;
  function SaveData(bp: PByte; ByteCount: UInt16; Stream: TStream): Boolean; inline;


  function LoadUInt64(Stream: TStream): UInt64; inline;
  function LoadUInt16(Stream: TStream): UInt16; inline;
  function BatchLoadUInt64(bp: PByte; Len: LongInt): UInt64; inline;
  function LoadWideString(Stream: TStream): WideString; inline;

implementation

uses
  SysUtils;

function BytesToSignedData(bp: PByte; ByteCount: Integer): Int64;
var
  Ptr: PByte;
  i: Integer;

begin
  Ptr := @Result;
  Inc(Ptr, SizeOf(Result));

  for i := 1 to ByteCount do
  begin
    Ptr^ := bp^;
    Dec(Ptr);
    Dec(bp);

  end;

end;


function BytesToUnSignedData(bp: PByte; ByteCount: Integer): UInt64;
var
  Ptr: PByte;
  i: Integer;

begin
  Ptr := @Result;
  Inc(Ptr, SizeOf(Result));

  for i := 1 to ByteCount do
  begin
    Ptr^ := bp^;
    Dec(Ptr);
    Dec(bp);

  end;

end;

function SaveWideString(constref w: WideString; Stream: TStream): Boolean;
begin
  SaveUInt16(Length(w), Stream);
  Result := SaveData(@w[1], SizeOf(WideChar) * Length(w), Stream);
end;

function SaveData(bp: PByte; ByteCount: UInt16; Stream: TStream): Boolean;
begin
  Result := Stream.Write(bp^, ByteCount) = ByteCount;

end;

function SaveInt8(constref n: Int8; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 1, Stream);

end;

function SaveInt16(constref n: Int16; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 2, Stream);

end;

function SaveInt32(constref n: Int32; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 4, Stream);

end;

function SaveInt64(constref n: Int64; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 8, Stream);

end;

function SaveUInt8(constref n: UInt8; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 1, Stream);

end;

function SaveUInt16(constref n: UInt16; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 2, Stream);

end;

function SaveUInt32(constref n: UInt32; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 4, Stream);

end;

function SaveUInt64(constref n: uInt64; Stream: TStream): Boolean;
begin
  Result := SaveData(@n, 8, Stream);

end;

function LoadUInt64(Stream: TStream): UInt64;
begin
  Stream.Read(Result, 8);

end;

function LoadUInt16(Stream: TStream): UInt16;
begin
  Stream.Read(Result, 2);

end;

function BatchLoadUInt64(bp: PByte; Len: LongInt): UInt64;
begin
  raise Exception.Create('NIY');

end;

function LoadWideString(Stream: TStream): WideString;
var
  l: SizeInt;

begin
  l := LoadUInt16(Stream);
  SetLength(Result, l);
  Stream.Read(Result[1], SizeOf(WideChar) * l);

end;

end.

