unit GenericCollection.UtilsUnit;

{$mode ObjFPC}{$H+}

interface

type
  TBytes = array of Byte;

  function Int8ToBytes(n: Int8): TBytes;
  function Int16ToBytes(n: Int16): TBytes;
  function Int32ToBytes(n: Int32): TBytes;
  function Int64ToBytes(n: Int64): TBytes;
  function UInt8ToBytes(n: UInt8): TBytes;
  function UInt16ToBytes(n: UInt16): TBytes;
  function UInt32ToBytes(n: UInt32): TBytes;
  function UInt64ToBytes(n: uInt64): TBytes;
  function DataToBytes(bp: PByte; ByteCount: Integer): TBytes;

implementation

function UIntToBytes(n: uInt64): TBytes;
begin
  Result := DataToBytes(PUInt8(@n), 8);

end;

function Int8ToBytes(n: Int8): TBytes;
begin
  Result := DataToBytes(PByte(@n), 1);


end;

function Int16ToBytes(n: Int16): TBytes;
begin
  Result := DataToBytes(PByte(@n), 2);

end;

function Int32ToBytes(n: Int32): TBytes;
begin
  Result := DataToBytes(PByte(@n), 4);

end;

function Int64ToBytes(n: Int64): TBytes;
begin
  Result := DataToBytes(PByte(@n), 8);

end;

function UInt8ToBytes(n: UInt8): TBytes;
begin
  Result := DataToBytes(PUInt8(@n), 1);

end;

function UInt16ToBytes(n: UInt16): TBytes;
begin
  Result := DataToBytes(PUInt8(@n), 2);

end;

function UInt32ToBytes(n: UInt32): TBytes;
begin
  Result := DataToBytes(PUInt8(@n), 4);

end;

function UInt64ToBytes(n: uInt64): TBytes;
begin
  Result := DataToBytes(PUInt8(@n), 8);

end;

function DataToBytes(bp: PUInt8; ByteCount: Integer): TBytes;
var
  i: Integer;
  Target: PByte;

begin
  SetLength(Result, ByteCount);
  Target := @Result[0];
  for i := 1 to ByteCount do
  begin
    Target^ := bp^;
    Inc(bp);
    Inc(Target);

  end;
end;

end.

