unit ProtoStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBitStream }

  TBitStreamReader = class(TObject)
  private
    FStream: TStream;
    CurrentByte: Byte;
    CurrentIndex: Integer;
    function GetPosition: Integer;
    function GetSize: Int64;

  public
    property Position: Integer read GetPosition;
    property Size: Int64 read GetSize;
    // Takes the ownership of AnStream Object.
    constructor Create(AnStream: TStream);
    destructor Destroy; override;

    function ReadNextBit: Integer;
    function ReadNextByte: Integer;

  end;

const
  ByteArraySize = 10;

type
  TByteArray = array [0..ByteArraySize - 1] of Byte;

  { TLinkListNode }
  // TODO(Amir): Change this class to record.

  TLinkListNode = class(TObject)
  private
    ID: Integer;
    FNext: TLinkListNode;
    FData: TByteArray;
    NextIndex: Integer;

    function GetByteAt(Index: Integer): Byte;
    function GetSize: Integer;
    function GetTotalSize: Integer;

  public
    property Next: TLinkListNode read FNext;
    property Size: Integer read GetSize;
    property TotalSize: Integer read GetTotalSize;
    property ByteAt[Index: Integer]: Byte read GetByteAt;

    constructor Create;

    function WriteRawData(p: Pointer; Count: Integer): TLinkListNode;
    procedure WriteLength(n: Uint32);

  end;

  { TProtoStreamWriter }

  TProtoStreamWriter = class(TObject)
  private
    FStream: TStream;
    FRoot: TLinkListNode;
    FCurrentNode: TLinkListNode;
    TakeOwnership: Boolean;

    function GetSize: Int64;
    property CurrentNode: TLinkListNode read FCurrentNode;
    procedure WriteToStream;

  public
    // Size in Bit.
    property Size: Int64 read GetSize;
    property Root: TLinkListNode read FRoot;

    constructor Create(AnStream: TStream; _TakeOwnership: Boolean = True);
    destructor Destroy; override;

    function AddIntervalNode: TLinkListNode;

    (* Encode and write varint. *)
    procedure WriteRawVarint32(Value: Integer);
    (* Encode and write varint. *)
    procedure WriteRawVarint64(Value: Int64);
    (* Encode and write tag. *)
    procedure WriteTag(FieldNumber: Integer; WireType: Integer);
    (* Encode and write a single byte. *)
    procedure WriteRawByte(value: Byte);
    (* Write the data with specified Count. *)
    procedure WriteRawData(const p: Pointer; Count: Integer);
    (* Write a double field, including tag. *)
    procedure WriteDouble(FieldNumber: Integer; Value: Double);
    (* Write a single field, including tag. *)
    procedure WriteFloat(FieldNumber: Integer; Value: Single);
    (* Write a int64 field, including tag. *)
    procedure WriteVarInt64(FieldNumber: Integer; Value: Int64);
    (* Write a varint32 field, including tag. *)
    procedure WriteVarInt32(FieldNumber: Integer; Value: Integer);
    (* Write a UInt64 field, including tag. *)
    procedure WriteVarUInt64(FieldNumber: Integer; Value: UInt64);
    (* Write a UInt32 field, including tag. *)
    procedure WriteVarUInt32(FieldNumber: Integer; Value: UInt32);
    (* Write a fixed32 field, including tag. *)
    procedure WriteFixed32(FieldNumber: Integer; Value: UInt32);
    (* Write a fixed64 field, including tag. *)
    procedure WriteFixed64(FieldNumber: Integer; Value: UInt64);
    (* Write a sfixed32 field, including tag. *)
    procedure WriteSFixed32(FieldNumber: Integer; Value: Int32);
    (* Write a fixed64 field, including tag. *)
    procedure WriteSFixed64(FieldNumber: Integer; Value: Int64);
    (* Write a Boolean field, including tag. *)
    procedure WriteBoolean(FieldNumber: Integer; Value: Boolean);
    (* Write a byte field, including tag. *)
    procedure WriteByte(FieldNumber: Integer; Value: Byte);
    (* Write a string field, including tag. *)
    procedure WriteString(FieldNumber: Integer; const Value: AnsiString);
//    (*  Write a unsigned int32 field, including tag. *)
//    procedure WriteUInt32(FieldNumber: Integer; Value: cardinal);

  end;

  { EInvalidInput }

  EInvalidInput = class(Exception)
    constructor Create;

  end;

  { TProtoStreamReader }

  TProtoStreamReader = class(TObject)
  private
    FStream: TStream;
    TakeOwnership: Boolean;

    function GetPosition: Int64;
    function GetSize: Int64;
    function ReadNextByte: Byte;

    procedure ReadRawData(P: Pointer; Count: Integer);

  public
    property Size: Int64 read GetSize;
    property Position: Int64 read GetPosition;

    constructor Create(AnStream: TStream; _TakeOwnership: Boolean = True);
    destructor Destroy; override;

    function ReadString: AnsiString;
    function ReadBool: Boolean;
    function ReadByte: Byte;
    function ReadDouble: Double;
    function ReadRawVarint64: Int64;
    function ReadRawVarint32: Int32;
    function ReadFloat: Single;
    function ReadVarInt32: Integer;
    function ReadVarInt64: Int64;
    function ReadVarUInt32: UInt32;
    function ReadVarUInt64: UInt64;
    function ReadFixed32: UInt32;
    function ReadFixed64: UInt64;
    function ReadSFixed32: Int32;
    function ReadSFixed64: Int64;

    (* Read and decode tag. *)
    procedure ReadTag(var FieldNumber, WireType: Integer);

  end;

const
  WIRETYPE_VARINT           = 0;
  WIRETYPE_FIXED64          = 1;
  WIRETYPE_LENGTH_DELIMITED = 2;
  WIRETYPE_START_GROUP      = 3;
  WIRETYPE_END_GROUP        = 4;
  WIRETYPE_FIXED32          = 5;

  TAG_TYPE_BITS = 3;
  TAG_TYPE_MASK = (1 shl TAG_TYPE_BITS) - 1;

implementation

function GetTagWireType(Tag: Integer): Integer;
begin
  Result := Tag and TAG_TYPE_MASK;
end;

function GetTagFieldNumber(Tag: Integer): Integer;
begin
  Result := Tag shr Tag_TYPE_BITS;
end;

function MakeTag(FieldNumber, WireType: Integer): Integer;
begin
  Result := (FieldNumber shl Tag_TYPE_BITS) or WireType;
end;

procedure DecodeTag(Tag: Integer; var FieldNumber, WireType: Integer);
begin
  FieldNumber := Tag shr TAG_TYPE_BITS;
  WireType := Tag and ((1 shl TAG_TYPE_BITS) - 1) ;
end;


{ EInvalidInput }

constructor EInvalidInput.Create;
begin
  inherited Create('Invalid Input');
end;

{ TProtoStreamReader }

function TProtoStreamReader.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TProtoStreamReader.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

function TProtoStreamReader.ReadNextByte: Byte;
begin
  FStream.Read(Result, 1);

end;

procedure TProtoStreamReader.ReadRawData(P: Pointer; Count: Integer);
begin
   FStream.Read(P^, Count);

end;

constructor TProtoStreamReader.Create(AnStream: TStream; _TakeOwnership: Boolean
  );
begin
  inherited Create;

  FStream := AnStream;
  TakeOwnership := _TakeOwnership;

end;

destructor TProtoStreamReader.Destroy;
begin
  if TakeOwnership then
    FStream.Free;

  inherited Destroy;
end;

function TProtoStreamReader.ReadString: AnsiString;
var
  l: Integer;

begin
  l := ReadRawVarint32;
  SetLength(Result, l);
  ReadRawData(PChar(Result), l);

end;

function TProtoStreamReader.ReadBool: Boolean;
var
  b: Byte;

begin
  Self.ReadRawData(@b, 1);
  Result := b <> 0;

end;

function TProtoStreamReader.ReadByte: Byte;
begin
  Self.ReadRawData(@Result, 1);

end;

function TProtoStreamReader.ReadRawVarint64: Int64;
var
  Shift: Integer;
  b: Int64;

begin
  Result := 0;
  Shift := 0;
  while Shift < 64 do
  begin
    if FStream.Size <= FStream.Position then
      raise EInvalidInput.Create();

    b := ReadNextByte;
    Result := Result or ((b and $7F) shl shift);
    if b < $80 then
	Break;
    Inc(Shift, 7);
  end;
end;

function TProtoStreamReader.ReadRawVarint32: Int32;
var
  Shift: Integer;
  b: Int32;

begin
  Result := 0;
  Shift := 0;
  while Shift < 32 do
  begin
    if FStream.Size <= FStream.Position then
      raise EInvalidInput.Create();

    b := ReadNextByte;
    Result := Result or ((b and $7F) shl shift);
    if b < $80 then
	Break;
    Inc(Shift, 7);
  end;
end;

function TProtoStreamReader.ReadFloat: Single;
begin
  Self.ReadRawData(@Result, SizeOf(Single));
end;

function TProtoStreamReader.ReadVarInt64: Int64;
begin
  Result := Self.ReadRawVarint64;

end;

function TProtoStreamReader.ReadVarInt32: Integer;
begin
  Result := Self.ReadRawVarint32;

end;

function TProtoStreamReader.ReadVarUInt64: UInt64;
begin
  Result := Self.ReadRawVarint64;

end;

function TProtoStreamReader.ReadFixed32: UInt32;
begin
  ReadRawData(@Result, SizeOf(UInt32));

end;

function TProtoStreamReader.ReadFixed64: UInt64;
begin
  ReadRawData(@Result, SizeOf(UInt64));

end;

function TProtoStreamReader.ReadSFixed32: Int32;
begin
  ReadRawData(@Result, SizeOf(Int32));

end;

function TProtoStreamReader.ReadSFixed64: Int64;
begin
  ReadRawData(@Result, SizeOf(Int64));

end;

function TProtoStreamReader.ReadVarUInt32: UInt32;
begin
  Result := Self.ReadRawVarint32;

end;

function TProtoStreamReader.ReadDouble: Double;
begin
  Self.ReadRawData(@Result, SizeOf(Double));

end;

procedure TProtoStreamReader.ReadTag(var FieldNumber, WireType: Integer);
var
  Tag: Int32;

begin
  Tag := ReadRawVarint32;
  DecodeTag(Tag, FieldNumber, WireType);

end;

{ TLinkListNode }

function TLinkListNode.GetByteAt(Index: Integer): Byte;
begin
  Result := FData[Index];

end;

function TLinkListNode.GetSize: Integer;
begin
  Result := NextIndex;
end;

function TLinkListNode.GetTotalSize: Integer;
var
  Node: TLinkListNode;

begin
  Result := 0;
  Node := Self;

  while Node <> nil do
  begin
    Inc(Result, Node.Size);

    Node := Node.Next;
  end;
end;

var
  CId: Integer;

constructor TLinkListNode.Create;
begin
  inherited;

  FNext := nil;
  NextIndex := 0;
  Inc(CId);
  ID := CID;
end;


function TLinkListNode.WriteRawData(p: Pointer; Count: Integer): TLinkListNode;
var
  Delta: Integer;

begin
  {
    NextIndex ... 1023
  }
  if NextIndex + Count <= ByteArraySize  then
  begin
    Move(p^, FData[NextIndex], Count);
    Inc(NextIndex, Count);
    Exit(Self);

  end;

  Delta := ByteArraySize - NextIndex;
  Move(p^, FData[NextIndex], Delta);
  NextIndex := ByteArraySize;
  FNext := TLinkListNode.Create;

  Exit(Next.WriteRawData(p + Delta, Count - Delta));

end;

procedure TLinkListNode.WriteLength(n: Uint32);
var
  b: Byte;

begin
  repeat
    b := n and $7F;
    n := n shr 7;
    if n <> 0 then
      b := b + $80;
    WriteRawData(@b, 1);
  until n = 0;

end;


{ TProtoStreamWriter }

function TProtoStreamWriter.GetSize: Int64;
var
  Node: TLinkListNode;

begin
  Result := 0;
  Node := Root;

  while Node <> nil do
  begin
    Inc(Result, Node.Size);

    Node := Node.Next;
  end;
end;

constructor TProtoStreamWriter.Create(AnStream: TStream; _TakeOwnership: Boolean
  );
begin
  inherited Create;

  FRoot := TLinkListNode.Create;
  FCurrentNode := Root;
  FStream := AnStream;
  TakeOwnership := _TakeOwnership;
end;

destructor TProtoStreamWriter.Destroy;
var
  Current, Next: TLinkListNode;

begin
  Self.WriteToStream;
  Current := Root;
  while Current <> nil do
  begin
    Next := Current.Next;
    Current.Free;
    Current := Next;

  end;

  if TakeOwnership then
    FStream.Free;

  inherited;

end;

procedure TProtoStreamWriter.WriteToStream;
var
  Node: TLinkListNode;

begin
  Node := Root;
  while Node <> nil do
  begin
    FStream.Write(Node.FData, Node.Size);

    Node := Node.Next;
  end;
end;

function TProtoStreamWriter.AddIntervalNode: TLinkListNode;
begin
  Assert(FCurrentNode.Next <> nil, 'FCurrentNode <> nil');
  Result := TLinkListNode.Create;
  FCurrentNode.FNext := Result;
  Result.FNext := TLinkListNode.Create;

  FCurrentNode := Result.FNext;

end;

procedure TProtoStreamWriter.WriteRawVarint32(Value: Integer);
var
  b: Byte;

begin
  repeat
    b := Value and $7F;
    Value := Value shr 7;
    if Value <> 0 then
      b := b + $80;
    WriteRawByte(b);
  until Value = 0;

end;

procedure TProtoStreamWriter.WriteRawVarint64(Value: Int64);
var
  b: Byte;

begin
  repeat
    b := Value and $7F;
    Value := Value shr 7;
    if Value <> 0 then
      b := b + $80;
    WriteRawByte(b);
  until Value = 0;
end;

procedure TProtoStreamWriter.WriteTag(FieldNumber: Integer; WireType: Integer);
begin
  if FieldNumber = -1 then
    Exit;
  WriteRawVarint32(MakeTag(FieldNumber, WireType));

end;

procedure TProtoStreamWriter.WriteRawByte(value: Byte);
begin
  WriteRawData(@value, 1);

end;

procedure TProtoStreamWriter.WriteRawData(const p: Pointer; Count: Integer);
begin
  FCurrentNode := CurrentNode.WriteRawData(p, Count);

end;

procedure TProtoStreamWriter.WriteDouble(FieldNumber: Integer; Value: Double);
begin
  WriteTag(FieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteFloat(FieldNumber: Integer; Value: Single);
begin
  WriteTag(FieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteVarInt64(FieldNumber: Integer; Value: Int64);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint64(Value);

end;

procedure TProtoStreamWriter.WriteVarInt32(FieldNumber: Integer; Value: Integer);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint32(Value);

end;

procedure TProtoStreamWriter.WriteVarUInt64(FieldNumber: Integer; Value: UInt64);
begin
  WriteVarInt64(FieldNumber, Value);

end;

procedure TProtoStreamWriter.WriteVarUInt32(FieldNumber: Integer; Value: UInt32);
begin
  WriteVarInt32(FieldNumber, Value);

end;

procedure TProtoStreamWriter.WriteFixed64(FieldNumber: Integer; Value: UInt64);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@value, SizeOf(value));

end;

procedure TProtoStreamWriter.WriteSFixed32(FieldNumber: Integer; Value: Int32);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@value, SizeOf(value));

end;

procedure TProtoStreamWriter.WriteSFixed64(FieldNumber: Integer; Value: Int64);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@value, SizeOf(value));

end;

procedure TProtoStreamWriter.WriteFixed32(FieldNumber: Integer; Value: UInt32);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@value, SizeOf(value));

end;

procedure TProtoStreamWriter.WriteBoolean(FieldNumber: Integer; Value: Boolean);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawByte(Ord(Value));

end;

procedure TProtoStreamWriter.WriteByte(FieldNumber: Integer; Value: Byte);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawByte(Value);

end;

procedure TProtoStreamWriter.WriteString(FieldNumber: Integer;
  const Value: AnsiString);
begin
  WriteTag(FieldNumber, WIRETYPE_LENGTH_DELIMITED);
  WriteRawVarint32(Length(Value));
  WriteRawData(PChar(Value), Length(Value));

end;

function TBitStreamReader.GetPosition: Integer;
begin
  Result := FStream.Position * 8 - (8 - CurrentIndex);

end;

function TBitStreamReader.GetSize: Int64;
begin
  Result := FStream.Size * 8;

end;

constructor TBitStreamReader.Create(AnStream: TStream);
begin
  inherited Create;

  FStream := AnStream;
  CurrentIndex := 0;
  FStream.Read(CurrentByte, 1);
end;

destructor TBitStreamReader.Destroy;
begin
  FStream.Free;

  inherited;

end;

function TBitStreamReader.ReadNextBit: Integer;
begin
  Result := (CurrentByte shr CurrentIndex) and 1;
  Inc(CurrentIndex);
  if (CurrentIndex = 8) and (FStream.Position < FStream.Size) then
  begin
    CurrentIndex := 0;
    FStream.ReadBuffer(CurrentByte, 1);

  end;
end;

function TBitStreamReader.ReadNextByte: Integer;
var
  i: Integer;
  P2: Integer;

begin
  Result := 0;
  P2 := 1;
  for i := 0 to 7 do
  begin
    if ReadNextBit = 1 then
      Result := Result or P2;
    Inc(P2, P2);
  end;
end;

end.

