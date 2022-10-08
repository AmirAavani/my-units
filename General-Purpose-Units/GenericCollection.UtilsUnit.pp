unit GenericCollection.UtilsUnit;

{$mode ObjFPC}{$H+}

interface
uses
  classes;

type
  TBytes = array of Byte;

  function SaveInt8(n: Int8; Stream: TStream): Boolean;
  function SaveInt16(n: Int16; Stream: TStream): Boolean;
  function SaveInt32(n: Int32; Stream: TStream): Boolean;
  function SaveInt64(n: Int64; Stream: TStream): Boolean;
  function SaveUInt8(n: UInt8; Stream: TStream): Boolean;
  function SaveUInt16(n: UInt16; Stream: TStream): Boolean;
  function SaveUInt32(n: UInt32; Stream: TStream): Boolean;
  function SaveUInt64(n: uInt64; Stream: TStream): Boolean;
  function SaveData(bp: PByte; ByteCount: UInt16; Stream: TStream): Boolean;


  function LoadUInt64(Stream: TStream): UInt64;
  function LoadUnsignedData(Stream: TStream; ByteCount: Integer): UInt64;
  function LoadSignedData(Stream: TStream; ByteCount: Integer): Int64;

implementation

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


function SaveData(bp: PByte; ByteCount: UInt16; Stream: TStream): Boolean;
begin
  Result := Stream.Write(bp^, ByteCount) = ByteCount;

end;

function SaveInt8(n: Int8; Stream: TStream): Boolean;
begin
  SaveData(@n, 1, Stream);

end;

function SaveInt16(n: Int16; Stream: TStream): Boolean;
begin
  SaveData(@n, 2, Stream);

end;

function SaveInt32(n: Int32; Stream: TStream): Boolean;
begin
  SaveData(@n, 4, Stream);

end;

function SaveInt64(n: Int64; Stream: TStream): Boolean;
begin
  SaveData(@n, 8, Stream);

end;

function SaveUInt8(n: UInt8; Stream: TStream): Boolean;
begin
  SaveData(@n, 1, Stream);

end;

function SaveUInt16(n: UInt16; Stream: TStream): Boolean;
begin
  SaveData(@n, 2, Stream);

end;

function SaveUInt32(n: UInt32; Stream: TStream): Boolean;
begin
  SaveData(@n, 4, Stream);

end;

function SaveUInt64(n: uInt64; Stream: TStream): Boolean;
begin
  SaveData(@n, 8, Stream);

end;

function LoadUInt64(Stream: TStream): UInt64;
begin
  Result := LoadUnsignedData(Stream, 8);
end;

function LoadUnsignedData(Stream: TStream; ByteCount: Integer): UInt64;
begin
  Stream.Read(Result, ByteCount);

end;

function LoadSignedData(Stream: TStream; ByteCount: Integer): Int64;
begin
  Stream.Read(Result, ByteCount);

end;

end.

