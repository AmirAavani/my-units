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
  ByteArraySize = 1;

type
  TByteArray = array [0..ByteArraySize - 1] of Byte;

  { TLinkListNode }

  TLinkListNode = class(TObject)
  private
    ID: Integer;
    FNext: TLinkListNode;
    FData: TByteArray;
    NextIndex: Integer;

    function GetByteAt(Index: Integer): Byte;
    function GetSize: Integer;

  public
    property Next: TLinkListNode read FNext;
    property Size: Integer read GetSize;
    property ByteAt[Index: Integer]: Byte read GetByteAt;

    constructor Create;
    destructor Destroy; override;

    function WriteRawData(p: Pointer; Count: Integer): TLinkListNode;

  end;

  { TProtoStreamWriter }

  TProtoStreamWriter = class(TObject)
  private
    FRoot: TLinkListNode;
    FCurrentNode: TLinkListNode;

    function GetSize: Int64;
    property CurrentNode: TLinkListNode read FCurrentNode;

  public
    // Size in Bit.
    property Size: Int64 read GetSize;
    property Root: TLinkListNode read FRoot;

    // Takes the ownership of AnStream Object.
    constructor Create;
    destructor Destroy; override;
    procedure WriteToStream(AnStream: TStream);

    (* Encode and write varint. *)
    procedure WriteRawVarint32(Value: Integer);
    (* Encode and write varint. *)
    procedure WriteRawVarint64(Value: int64);
    (* Encode and write tag. *)
    procedure WriteTag(FieldNumber: Integer; WireType: Integer);
    (* Encode and write single byte. *)
    procedure WriteRawByte(value: Byte);
    (* Write the data with specified Count. *)
    procedure WriteRawData(const p: Pointer; Count: Integer);
    (* Write a double field, including tag. *)
    procedure WriteDouble(FieldNumber: Integer; Value: double);
    (* Write a single field, including tag. *)
    procedure WriteFloat(FieldNumber: Integer; Value: single);
    (* Write a int64 field, including tag. *)
    procedure WriteInt64(FieldNumber: Integer; Value: int64);
    (* Write a int32 field, including tag. *)
    procedure WriteInt32(FieldNumber: Integer; Value: Integer);
    (* Write a UInt64 field, including tag. *)
    procedure WriteUInt64(FieldNumber: Integer; Value: UInt64);
    (* Write a UInt32 field, including tag. *)
    procedure WriteUInt32(FieldNumber: Integer; Value: UInt32);
    (* Write a fixed64 field, including tag. *)
    procedure WriteFixed64(FieldNumber: Integer; Value: int64);
    (* Write a fixed32 field, including tag. *)
    procedure WriteFixed32(FieldNumber: Integer; Value: Integer);
    (* Write a boolean field, including tag. *)
    procedure WriteBoolean(FieldNumber: Integer; Value: boolean);
    (* Write a string field, including tag. *)
    procedure WriteString(FieldNumber: Integer; const Value: AnsiString);
//    (*  Write a unsigned int32 field, including tag. *)
//    procedure WriteUInt32(FieldNumber: Integer; Value: cardinal);

  end;

implementation

const
  WIRETYPE_VARINT           = 0;
  WIRETYPE_FIXED64          = 1;
  WIRETYPE_LENGTH_DELIMITED = 2;
  WIRETYPE_START_GROUP      = 3;
  WIRETYPE_END_GROUP        = 4;
  WIRETYPE_FIXED32          = 5;

  TAG_TYPE_BITS = 3;
  TAG_TYPE_MASK = (1 shl TAG_TYPE_BITS) - 1;

  RecursionLimit = 64;

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
  Result := (FieldNumber shl Tag_TYPE_BITS) or wireType;
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

var
  dID: Integer;
constructor TLinkListNode.Create;
begin
  inherited;

  FNext := nil;
  NextIndex := 0;
  Inc(dID);
  ID := dID;
end;

destructor TLinkListNode.Destroy;
begin
  FNext.Free;

  inherited Destroy;
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

constructor TProtoStreamWriter.Create;
begin
  inherited Create;

  FRoot := TLinkListNode.Create;
  FCurrentNode := Root;
end;

destructor TProtoStreamWriter.Destroy;
begin
  Root.Free;

  inherited;

end;

procedure TProtoStreamWriter.WriteToStream(AnStream: TStream);
var
  Node: TLinkListNode;

begin
  Node := Root;

  while Node <> nil do
  begin
    AnStream.Write(Node.FData, Node.Size);

    Node := Node.Next;
  end;

  AnStream.Free;
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

procedure TProtoStreamWriter.WriteRawVarint64(Value: int64);
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

procedure TProtoStreamWriter.WriteDouble(FieldNumber: Integer; Value: double);
begin
  WriteTag(FieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteFloat(FieldNumber: Integer; Value: single);
begin
  WriteTag(FieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteInt64(FieldNumber: Integer; Value: int64);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint64(Value);

end;

procedure TProtoStreamWriter.WriteInt32(FieldNumber: Integer; Value: Integer);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint32(Value);

end;

procedure TProtoStreamWriter.WriteUInt64(FieldNumber: Integer; Value: UInt64);
begin
  Self.WriteInt64(FieldNumber, Value);

end;

procedure TProtoStreamWriter.WriteUInt32(FieldNumber: Integer; Value: UInt32);
begin
  Self.WriteInt32(FieldNumber, Value);

end;

procedure TProtoStreamWriter.WriteFixed64(FieldNumber: Integer; Value: int64);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@value, SizeOf(value));

end;

procedure TProtoStreamWriter.WriteFixed32(FieldNumber: Integer; Value: Integer);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@value, SizeOf(value));

end;

procedure TProtoStreamWriter.WriteBoolean(FieldNumber: Integer; Value: boolean);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawByte(Ord(Value));

end;

procedure TProtoStreamWriter.WriteString(FieldNumber: Integer;
  const Value: AnsiString);
begin
  WriteTag(FieldNumber, WIRETYPE_LENGTH_DELIMITED);
  WriteRawVarint32(Length(Value));
  WriteRawData(PChar(Value), Length(Value));

end;

{
procedure TProtoStreamWriter.WriteUInt32(FieldNumber: Integer; value: cardinal);
begin
  writeTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint32(Value);

end;
}

{
procedure TProtoBufOutput.writeMessage(fieldNumber: integer;
  const value: IpbMessage);
begin
  writeTag(fieldNumber, WIRETYPE_LENGTH_DELIMITED);
  writeRawVarint32(value.getSerializedSize());
  value.writeTo(self);
end;
}

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

