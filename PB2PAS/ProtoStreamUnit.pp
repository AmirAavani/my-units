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
    CurrentByte: byte;
    CurrentIndex: integer;
    function GetPosition: integer;
    function GetSize: int64;

  public
    property Position: integer read GetPosition;
    property Size: int64 read GetSize;
    // Takes the ownership of AnStream Object.
    constructor Create(AnStream: TStream);
    destructor Destroy; override;

    function ReadNextBit: integer;
    function ReadNextByte: integer;

  end;

const
  ByteArraySize = 4096;

type
  TByteArray = array [0..ByteArraySize - 1] of byte;

  { TLinkListNode }
  // TODO(Amir): Change this class to record.

  TLinkListNode = class(TObject)
  private
    ID: integer;
    FNext: TLinkListNode;
    FData: TByteArray;
    NextIndex: integer;

    function GetByteAt(Index: integer): byte;
    function GetSize: integer;
    function GetTotalSize: integer;

  public
    property Next: TLinkListNode read FNext;
    property Size: integer read GetSize;
    property TotalSize: integer read GetTotalSize;
    property ByteAt[Index: integer]: byte read GetByteAt;

    constructor Create;

    function WriteRawData(p: Pointer; Count: integer): TLinkListNode;
    procedure WriteLength(n: uint32);

  end;

  { TProtoStreamWriter }

  TProtoStreamWriter = class(TObject)
  private
    FStream: TStream;
    FRoot: TLinkListNode;
    FCurrentNode: TLinkListNode;
    TakeOwnership: boolean;

    function GetSize: int64;
    property CurrentNode: TLinkListNode read FCurrentNode;

  protected
    procedure WriteToStream;

  public
    // Size in Bit.
    property Size: int64 read GetSize;
    property Root: TLinkListNode read FRoot;

    constructor Create(AnStream: TStream; _TakeOwnership: boolean = True);
    destructor Destroy; override;

    function AddIntervalNode: TLinkListNode;

    (* Encode and write varint. *)
    procedure WriteRawVarint32(Value: integer); inline;
    (* Encode and write varint. *)
    procedure WriteRawVarint64(Value: int64); inline;
    (* Encode and write tag. *)
    procedure WriteTag(FieldNumber: integer; WireType: integer); inline;
    (* Encode and write a single byte. *)
    procedure WriteRawByte(Value: byte); inline;
    (* Write the data with specified Count. *)
    procedure WriteRawData(const p: Pointer; Count: integer); inline;
    (* Write a double field, including tag. *)
    procedure WriteDouble(FieldNumber: integer; Value: double); inline;
    (* Write a single field, including tag. *)
    procedure WriteFloat(FieldNumber: integer; Value: single); inline;
    (* Write a int64 field, including tag. *)
    procedure WriteVarInt64(FieldNumber: integer; Value: int64); inline;
    (* Write a varint32 field, including tag. *)
    procedure WriteVarInt32(FieldNumber: integer; Value: integer); inline;
    (* Write a UInt64 field, including tag. *)
    procedure WriteVarUInt64(FieldNumber: integer; Value: uint64); inline;
    (* Write a UInt32 field, including tag. *)
    procedure WriteVarUInt32(FieldNumber: integer; Value: uint32); inline;
    (* Write a fixed32 field, including tag. *)
    procedure WriteFixed32(FieldNumber: integer; Value: uint32); inline;
    (* Write a fixed64 field, including tag. *)
    procedure WriteFixed64(FieldNumber: integer; Value: uint64); inline;
    (* Write a sfixed32 field, including tag. *)
    procedure WriteSFixed32(FieldNumber: integer; Value: int32); inline;
    (* Write a fixed64 field, including tag. *)
    procedure WriteSFixed64(FieldNumber: integer; Value: int64); inline;
    (* Write a Boolean field, including tag. *)
    procedure WriteBoolean(FieldNumber: integer; Value: boolean); inline;
    (* Write a byte field, including tag. *)
    procedure WriteByte(FieldNumber: integer; Value: byte); inline;
    (* Write a string field, including tag. *)
    procedure WriteString(FieldNumber: integer; const Value: ansistring); inline;
    //    (*  Write a unsigned int32 field, including tag. *)
    //    procedure WriteUInt32(FieldNumber: Integer; Value: cardinal);

    procedure Clear; inline;

  end;

  { TProtoStreamReader }

  TProtoStreamReader = class(TObject)
  private
    FStream: TStream;
    TakeOwnership: boolean;

    function GetPosition: int64;
    function GetSize: int64;
    function ReadNextByte: byte;

    procedure ReadRawData(P: Pointer; Count: integer);
    procedure SetPosition(AValue: int64);

  public
    property Size: int64 read GetSize;
    property Position: int64 read GetPosition write SetPosition;

    constructor Create(AnStream: TStream; _TakeOwnership: boolean = True);
    destructor Destroy; override;

    function ReadString: ansistring;
    function ReadBool: boolean;
    function ReadByte: byte;
    function ReadDouble: double;
    function ReadRawVarint64: int64;
    function ReadRawVarint32: int32;
    function ReadFloat: single;
    function ReadVarInt32: integer;
    function ReadVarInt64: int64;
    function ReadVarUInt32: uint32;
    function ReadVarUInt64: uint64;
    function ReadFixed32: uint32;
    function ReadFixed64: uint64;
    function ReadSFixed32: int32;
    function ReadSFixed64: int64;

    (* Read and decode tag. *)
    procedure ReadTag(var FieldNumber, WireType: integer);

  end;

const
  WIRETYPE_VARINT = 0;
  WIRETYPE_FIXED64 = 1;
  WIRETYPE_LENGTH_DELIMITED = 2;
  WIRETYPE_START_GROUP = 3;
  WIRETYPE_END_GROUP = 4;
  WIRETYPE_FIXED32 = 5;

  TAG_TYPE_BITS = 3;
  TAG_TYPE_MASK = (1 shl TAG_TYPE_BITS) - 1;

implementation

function GetTagWireType(Tag: integer): integer;
begin
  Result := Tag and TAG_TYPE_MASK;
end;

function GetTagFieldNumber(Tag: integer): integer;
begin
  Result := Tag shr Tag_TYPE_BITS;
end;

function MakeTag(FieldNumber, WireType: integer): integer;
begin
  Result := (FieldNumber shl Tag_TYPE_BITS) or WireType;
end;

procedure DecodeTag(Tag: integer; var FieldNumber, WireType: integer);
begin
  FieldNumber := Tag shr TAG_TYPE_BITS;
  WireType := Tag and ((1 shl TAG_TYPE_BITS) - 1);
end;


{ TProtoStreamReader }

function TProtoStreamReader.GetSize: int64;
begin
  Result := FStream.Size;
end;

function TProtoStreamReader.GetPosition: int64;
begin
  Result := FStream.Position;
end;

function TProtoStreamReader.ReadNextByte: byte;
var
  BytesRead: integer;
begin
  BytesRead := FStream.Read(Result, 1);
  if BytesRead = 0 then
    Result := 0;  // Return 0 on EOF instead of undefined value
end;

procedure TProtoStreamReader.ReadRawData(P: Pointer; Count: integer);
begin
  FStream.Read(P^, Count);

end;

procedure TProtoStreamReader.SetPosition(AValue: int64);
begin
  Self.FStream.Position := AValue;

end;

constructor TProtoStreamReader.Create(AnStream: TStream; _TakeOwnership: boolean);
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

function TProtoStreamReader.ReadString: ansistring;
var
  l: integer;
begin
  l := ReadRawVarint32;
  SetLength(Result, l);
  ReadRawData(PChar(Result), l);

end;

function TProtoStreamReader.ReadBool: boolean;
var
  b: byte;
begin
  Self.ReadRawData(@b, 1);
  Result := b <> 0;

end;

function TProtoStreamReader.ReadByte: byte;
begin
  Self.ReadRawData(@Result, 1);

end;

function TProtoStreamReader.ReadRawVarint64: int64;
var
  Shift: integer;
  b: int64;
begin
  Result := 0;
  Shift := 0;
  while Shift < 64 do
  begin
    { PERFORMANCE FIX: Removed GetSize check that caused ~1M lseek() syscalls
      The buffered stream will return 0 bytes on EOF, which is sufficient
      for detecting end-of-stream without expensive system calls }

    b := ReadNextByte;
    Result := Result or ((b and $7F) shl shift);
    if b < $80 then
      Break;
    Inc(Shift, 7);
  end;
end;

function TProtoStreamReader.ReadRawVarint32: int32;
var
  Shift: integer;
  b: int32;
begin
  Result := 0;
  Shift := 0;
  while Shift < 32 do
  begin
    { PERFORMANCE FIX: Removed GetSize check that caused ~1M lseek() syscalls
      The buffered stream will return 0 bytes on EOF, which is sufficient
      for detecting end-of-stream without expensive system calls }

    b := ReadNextByte;
    Result := Result or ((b and $7F) shl shift);
    if b < $80 then
      Break;
    Inc(Shift, 7);
  end;
end;

function TProtoStreamReader.ReadFloat: single;
begin
  Self.ReadRawData(@Result, SizeOf(single));
end;

function TProtoStreamReader.ReadVarInt64: int64;
begin
  Result := Self.ReadRawVarint64;

end;

function TProtoStreamReader.ReadVarInt32: integer;
begin
  Result := Self.ReadRawVarint32;

end;

function TProtoStreamReader.ReadVarUInt64: uint64;
begin
  Result := Self.ReadRawVarint64;

end;

function TProtoStreamReader.ReadFixed32: uint32;
begin
  ReadRawData(@Result, SizeOf(uint32));

end;

function TProtoStreamReader.ReadFixed64: uint64;
begin
  ReadRawData(@Result, SizeOf(uint64));

end;

function TProtoStreamReader.ReadSFixed32: int32;
begin
  ReadRawData(@Result, SizeOf(int32));

end;

function TProtoStreamReader.ReadSFixed64: int64;
begin
  ReadRawData(@Result, SizeOf(int64));

end;

function TProtoStreamReader.ReadVarUInt32: uint32;
begin
  Result := Self.ReadRawVarint32;

end;

function TProtoStreamReader.ReadDouble: double;
begin
  Self.ReadRawData(@Result, SizeOf(double));

end;

procedure TProtoStreamReader.ReadTag(var FieldNumber, WireType: integer);
var
  Tag: int32;
begin
  Tag := ReadRawVarint32;
  DecodeTag(Tag, FieldNumber, WireType);

end;

{ TLinkListNode }

function TLinkListNode.GetByteAt(Index: integer): byte;
begin
  Result := FData[Index];

end;

function TLinkListNode.GetSize: integer;
begin
  Result := NextIndex;
end;

function TLinkListNode.GetTotalSize: integer;
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
  CId: integer;

constructor TLinkListNode.Create;
begin
  inherited;

  FNext := nil;
  NextIndex := 0;
  Inc(CId);
  ID := CID;
end;


function TLinkListNode.WriteRawData(p: Pointer; Count: integer): TLinkListNode;
var
  Delta: integer;
begin
  {
    NextIndex ... 1023
  }
  if NextIndex + Count <= ByteArraySize then
  begin
    Move(p^, FData[NextIndex], Count);
    Inc(NextIndex, Count);
    Exit(Self);

  end;

  Delta := ByteArraySize - NextIndex;
  Move(p^, FData[NextIndex], Delta);
  NextIndex := ByteArraySize;
  FNext := TLinkListNode.Create;

  Exit(Next.WriteRawData(pbyte(p) + Delta, Count - Delta));

end;

procedure TLinkListNode.WriteLength(n: uint32);
var
  b: byte;
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

function TProtoStreamWriter.GetSize: int64;
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

constructor TProtoStreamWriter.Create(AnStream: TStream; _TakeOwnership: boolean);
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
    if Node.Size > 0 then
      FStream.Write(Node.FData[0], Node.Size);

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

procedure TProtoStreamWriter.WriteRawVarint32(Value: integer);
var
  b: byte;
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
  b: byte;
begin
  repeat
    b := Value and $7F;
    Value := Value shr 7;
    if Value <> 0 then
      b := b + $80;
    WriteRawByte(b);
  until Value = 0;
end;

procedure TProtoStreamWriter.WriteTag(FieldNumber: integer; WireType: integer);
begin
  if FieldNumber = -1 then
    Exit;
  WriteRawVarint32(MakeTag(FieldNumber, WireType));

end;

procedure TProtoStreamWriter.WriteRawByte(Value: byte);
begin
  WriteRawData(@Value, 1);

end;

procedure TProtoStreamWriter.WriteRawData(const p: Pointer; Count: integer);
begin
  FCurrentNode := CurrentNode.WriteRawData(p, Count);

end;

procedure TProtoStreamWriter.WriteDouble(FieldNumber: integer; Value: double);
begin
  WriteTag(FieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteFloat(FieldNumber: integer; Value: single);
begin
  WriteTag(FieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteVarInt64(FieldNumber: integer; Value: int64);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint64(Value);

end;

procedure TProtoStreamWriter.WriteVarInt32(FieldNumber: integer; Value: integer);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawVarint32(Value);

end;

procedure TProtoStreamWriter.WriteVarUInt64(FieldNumber: integer; Value: uint64);
begin
  WriteVarInt64(FieldNumber, Value);

end;

procedure TProtoStreamWriter.WriteVarUInt32(FieldNumber: integer; Value: uint32);
begin
  WriteVarInt32(FieldNumber, Value);

end;

procedure TProtoStreamWriter.WriteFixed64(FieldNumber: integer; Value: uint64);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteSFixed32(FieldNumber: integer; Value: int32);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteSFixed64(FieldNumber: integer; Value: int64);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED64);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteFixed32(FieldNumber: integer; Value: uint32);
begin
  WriteTag(fieldNumber, WIRETYPE_FIXED32);
  WriteRawData(@Value, SizeOf(Value));

end;

procedure TProtoStreamWriter.WriteBoolean(FieldNumber: integer; Value: boolean);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawByte(Ord(Value));

end;

procedure TProtoStreamWriter.WriteByte(FieldNumber: integer; Value: byte);
begin
  WriteTag(FieldNumber, WIRETYPE_VARINT);
  WriteRawByte(Value);

end;

procedure TProtoStreamWriter.WriteString(FieldNumber: integer; const Value: ansistring);
begin
  WriteTag(FieldNumber, WIRETYPE_LENGTH_DELIMITED);
  WriteRawVarint32(Length(Value));
  WriteRawData(PChar(Value), Length(Value));

end;

procedure TProtoStreamWriter.Clear;
var
  Current, Next: TLinkListNode;
begin
  Current := FRoot.FNext;
  while Current <> nil do
  begin
    Next := Current.FNext;
    Current.Free;
    Current := Next;
  end;

  FRoot.FNext := nil;
  FRoot.NextIndex := 0;
  FCurrentNode := FRoot;
end;

function TBitStreamReader.GetPosition: integer;
begin
  Result := FStream.Position * 8 - (8 - CurrentIndex);

end;

function TBitStreamReader.GetSize: int64;
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

function TBitStreamReader.ReadNextBit: integer;
begin
  Result := (CurrentByte shr CurrentIndex) and 1;
  Inc(CurrentIndex);
  if (CurrentIndex = 8) and (FStream.Position < FStream.Size) then
  begin
    CurrentIndex := 0;
    FStream.ReadBuffer(CurrentByte, 1);

  end;
end;

function TBitStreamReader.ReadNextByte: integer;
var
  i: integer;
  P2: integer;
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
