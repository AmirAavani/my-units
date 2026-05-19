unit ProtoHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperListsUnit, ProtoStreamUnit;

type
  TBytes = specialize TSimpleTypeList<byte>;
  TSingles = specialize TSimpleTypeList<single>;
  TDoubles = specialize TSimpleTypeList<double>;
  TInt32s = specialize TSimpleTypeList<int32>;
  TInt64s = specialize TSimpleTypeList<int64>;
  TUInt32s = specialize TSimpleTypeList<uint32>;
  TUInt64s = specialize TSimpleTypeList<uint64>;
  TBooleans = specialize TSimpleTypeList<boolean>;
  TAnsiStrings = specialize TSimpleTypeList<ansistring>;

  TBaseMessage = class;
  TBaseOneOf = class;

  { TBaseMessage }

  TBaseMessage = class(TObject)
  protected

    procedure SaveToStream(Stream: TProtoStreamWriter); virtual; abstract;
    function LoadFromStream(Stream: TProtoStreamReader; Len: integer): boolean;
      virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;

    function LoadFromStream(Stream: TStream): boolean; virtual;
    procedure SaveToStream(Stream: TStream); virtual;


    function ToJSON: ansistring; virtual;

  end;

  { TBaseOneOf }

  TBaseOneOf = class(TObject)
  protected
    _ObjectIndex: integer;
    _Data: Pointer;

    function GetPointerByIndex(Index: integer): Pointer;
    procedure SetPointerByIndex(Index: integer; AValue: Pointer);
  public
    property PointerByIndex[Index: integer]: Pointer
      read GetPointerByIndex write SetPointerByIndex;

    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;

  end;

  { EBaseOneOf }

  EBaseOneOf = class(Exception)
  public
    constructor CreateTwoValueAreSet;
  end;

procedure SaveFloat(Stream: TProtoStreamWriter; const Data: single;
  const TagID: integer);
procedure SaveDouble(Stream: TProtoStreamWriter; const Data: double;
  const TagID: integer);
procedure SaveInt32(Stream: TProtoStreamWriter; const Data: int32;
  const TagID: integer);
procedure SaveInt64(Stream: TProtoStreamWriter; const Data: int64;
  const TagID: integer);
procedure SaveUInt32(Stream: TProtoStreamWriter; const Data: uint32;
  const TagID: integer);
procedure SaveUInt64(Stream: TProtoStreamWriter; const Data: uint64;
  const TagID: integer);
procedure SaveSInt32(Stream: TProtoStreamWriter; const Data: int32;
  const TagID: integer);
procedure SaveSInt64(Stream: TProtoStreamWriter; const Data: int64;
  const TagID: integer);
procedure SaveFixed32(Stream: TProtoStreamWriter; const Data: uint32;
  const TagID: integer);
procedure SaveFixed64(Stream: TProtoStreamWriter; const Data: uint64;
  const TagID: integer);
procedure SaveSFixed32(Stream: TProtoStreamWriter; const Data: int32;
  const TagID: integer);
procedure SaveSFixed64(Stream: TProtoStreamWriter; const Data: int64;
  const TagID: integer);
procedure SaveString(Stream: TProtoStreamWriter; const Data: ansistring;
  const TagID: integer);
procedure SaveBool(Stream: TProtoStreamWriter; const Data: boolean;
  const TagID: integer);
procedure SaveByte(Stream: TProtoStreamWriter; const Data: byte; const TagID: integer);

function LoadFloat(Stream: TProtoStreamReader): single;
function LoadDouble(Stream: TProtoStreamReader): double;
function LoadInt32(Stream: TProtoStreamReader): int32;
function LoadInt64(Stream: TProtoStreamReader): int64;
function LoadUInt32(Stream: TProtoStreamReader): uint32;
function LoadUInt64(Stream: TProtoStreamReader): uint64;
function LoadSInt32(Stream: TProtoStreamReader): int32;
function LoadSInt64(Stream: TProtoStreamReader): int64;
function LoadFixed32(Stream: TProtoStreamReader): uint32;
function LoadFixed64(Stream: TProtoStreamReader): uint64;
function LoadSFixed32(Stream: TProtoStreamReader): int32;
function LoadSFixed64(Stream: TProtoStreamReader): int64;
function LoadString(Stream: TProtoStreamReader): ansistring;
function LoadBool(Stream: TProtoStreamReader): boolean;
function LoadByte(Stream: TProtoStreamReader): byte;
function SkipField(Stream: TProtoStreamReader; WireType: integer): boolean;

// TODO(Amir): Maybe replace this methods with a generic function.
procedure SaveRepeatedFloat(Stream: TProtoStreamWriter; const Data: TSingles;
  const TagID: integer);
procedure SaveRepeatedDouble(Stream: TProtoStreamWriter; const Data: TDoubles;
  const TagID: integer);
procedure SaveRepeatedInt32(Stream: TProtoStreamWriter; const Data: TInt32s;
  const TagID: integer);
procedure SaveRepeatedInt64(Stream: TProtoStreamWriter; const Data: TInt64s;
  const TagID: integer);
procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter; const Data: TUInt32s;
  const TagID: integer);
procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter; const Data: TUInt64s;
  const TagID: integer);
procedure SaveRepeatedSInt32(Stream: TProtoStreamWriter; const Data: TInt32s;
  const TagID: integer);
procedure SaveRepeatedSInt64(Stream: TProtoStreamWriter; const Data: TInt64s;
  const TagID: integer);
procedure SaveRepeatedFixed32(Stream: TProtoStreamWriter; const Data: TUInt32s;
  const TagID: integer);
procedure SaveRepeatedFixed64(Stream: TProtoStreamWriter; const Data: TUInt64s;
  const TagID: integer);
procedure SaveRepeatedSFixed32(Stream: TProtoStreamWriter; const Data: TInt32s;
  const TagID: integer);
procedure SaveRepeatedSFixed64(Stream: TProtoStreamWriter; const Data: TInt64s;
  const TagID: integer);
procedure SaveRepeatedString(Stream: TProtoStreamWriter; const Data: TAnsiStrings;
  const TagID: integer);
procedure SaveRepeatedBool(Stream: TProtoStreamWriter; const Data: TBooleans;
  const TagID: integer);
procedure SaveRepeatedByte(Stream: TProtoStreamWriter; const Data: TBytes;
  const TagID: integer);

// TODO(Amir): Maybe replace this methods with a generic function.
function LoadRepeatedFloat(Stream: TProtoStreamReader; Data: TSingles): boolean;
function LoadRepeatedDouble(Stream: TProtoStreamReader; Data: TDoubles): boolean;
function LoadRepeatedInt32(Stream: TProtoStreamReader; Data: TInt32s): boolean;
function LoadRepeatedInt64(Stream: TProtoStreamReader; Data: TInt64s): boolean;
function LoadRepeatedUInt32(Stream: TProtoStreamReader; Data: TUInt32s): boolean;
function LoadRepeatedUInt64(Stream: TProtoStreamReader; Data: TUInt64s): boolean;
function LoadRepeatedSint32(Stream: TProtoStreamReader; Data: TInt32s): boolean;
function LoadRepeatedSint64(Stream: TProtoStreamReader; Data: TInt64s): boolean;
function LoadRepeatedFixed32(Stream: TProtoStreamReader; Data: TUInt32s): boolean;
function LoadRepeatedFixed64(Stream: TProtoStreamReader; Data: TUInt64s): boolean;
function LoadRepeatedSFixed32(Stream: TProtoStreamReader; Data: TInt32s): boolean;
function LoadRepeatedSFixed64(Stream: TProtoStreamReader; Data: TInt64s): boolean;
function LoadRepeatedString(Stream: TProtoStreamReader; Data: TAnsiStrings): boolean;
function LoadRepeatedBool(Stream: TProtoStreamReader; Data: TBooleans): boolean;
function LoadRepeatedByte(Stream: TProtoStreamReader; Data: TBytes): boolean;

procedure SaveMessage(Stream: TProtoStreamWriter; const Data: TBaseMessage;
  const TagID: integer);
function LoadMessage(Stream: TProtoStreamReader; Data: TBaseMessage): boolean;
generic procedure SaveRepeatedMessage<TMessage>(Stream: TProtoStreamWriter;
  const Data: specialize TObjectList<TMessage>; const TagID: integer);
generic function LoadRepeatedMessage<TMessage>(Stream: TProtoStreamReader;
  Data: specialize TObjectList<TMessage>): boolean;

procedure MaybeDispose(P: PDouble);
procedure MaybeDispose(P: PSingle);
procedure MaybeDispose(P: PInt16);
procedure MaybeDispose(P: PInt32);
procedure MaybeDispose(P: PInt64);
procedure MaybeDispose(P: PUInt16);
procedure MaybeDispose(P: PUInt32);
procedure MaybeDispose(P: PUInt64);
procedure MaybeDispose(P: PBoolean);
procedure MaybeDispose(P: PAnsiString);
procedure MaybeDispose(P: pbyte);

implementation

uses
  fpjsonrtti;

  { EBaseOneOf }

constructor EBaseOneOf.CreateTwoValueAreSet;
begin
  inherited Create('Two values of an OneOf Field are set!');

end;

{ TBaseOneOf }

function TBaseOneOf.GetPointerByIndex(Index: integer): Pointer;
begin
  if Self = nil then
    Exit(nil);

  if Index = _ObjectIndex then
    Exit(_Data);

  Result := nil;

end;

procedure TBaseOneOf.SetPointerByIndex(Index: integer; AValue: Pointer);
begin
  if Self = nil then
    Exit;

  if _ObjectIndex = -1 then
  begin
    _Data := AValue;
    if AValue = nil then
      Exit;
    _ObjectIndex := Index;
    Exit;

  end;

  Clear;
  Self.SetPointerByIndex(Index, AValue);
end;

constructor TBaseOneOf.Create;
begin
  inherited Create;

  _ObjectIndex := -1;
end;

destructor TBaseOneOf.Destroy;
begin

  inherited Destroy;
end;

procedure TBaseOneOf.Clear;
begin

  _ObjectIndex := -1;
end;

{ TBaseMessage }

procedure SaveRepeatedSInt32(Stream: TProtoStreamWriter; const Data: TInt32s;
  const TagID: integer);
var
  SingleData: int32;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveSInt32(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedSInt64(Stream: TProtoStreamWriter; const Data: TInt64s;
  const TagID: integer);
var
  SingleData: int64;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveSInt64(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedFixed32(Stream: TProtoStreamWriter; const Data: TUInt32s;
  const TagID: integer);
var
  SingleData: uint32;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveFixed32(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedFixed64(Stream: TProtoStreamWriter; const Data: TUInt64s;
  const TagID: integer);
var
  SingleData: uint64;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveFixed64(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedSFixed32(Stream: TProtoStreamWriter; const Data: TInt32s;
  const TagID: integer);
var
  SingleData: int32;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveSFixed32(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedSFixed64(Stream: TProtoStreamWriter; const Data: TInt64s;
  const TagID: integer);
var
  SingleData: uint64;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveSFixed64(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedString(Stream: TProtoStreamWriter; const Data: TAnsiStrings;
  const TagID: integer);
var
  SingleData: ansistring;
begin
  if Data = nil then
    Exit;

  for SingleData in Data do
  begin
    SaveString(Stream, SingleData, TagID);

  end;

end;

procedure SaveRepeatedBool(Stream: TProtoStreamWriter; const Data: TBooleans;
  const TagID: integer);
var
  SingleData: boolean;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveBool(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedByte(Stream: TProtoStreamWriter; const Data: TBytes;
  const TagID: integer);
var
  SingleData: byte;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    SaveByte(Stream, SingleData, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

function LoadRepeatedFloat(Stream: TProtoStreamReader; Data: TSingles): boolean;
var
  Len: uint32;
  NewDatum: single;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadFloat(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedDouble(Stream: TProtoStreamReader; Data: TDoubles): boolean;
var
  Len: uint32;
  NewDatum: double;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadDouble(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedInt32(Stream: TProtoStreamReader; Data: TInt32s): boolean;
var
  Len: uint32;
  NewDatum: int32;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadInt32(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedInt64(Stream: TProtoStreamReader; Data: TInt64s): boolean;
var
  Len: uint32;
  NewDatum: int64;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadInt64(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedUInt32(Stream: TProtoStreamReader; Data: TUInt32s): boolean;
var
  Len: uint32;
  NewDatum: uint32;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadUInt32(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedUInt64(Stream: TProtoStreamReader; Data: TUInt64s): boolean;
var
  Len: uint32;
  NewDatum: uint64;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadUInt64(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedSint32(Stream: TProtoStreamReader; Data: TInt32s): boolean;
var
  Len: uint32;
  NewDatum: int32;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadSInt32(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedSint64(Stream: TProtoStreamReader; Data: TInt64s): boolean;
var
  Len: uint32;
  NewDatum: int64;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadSInt64(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedFixed32(Stream: TProtoStreamReader; Data: TUInt32s): boolean;
var
  Len: uint32;
  NewDatum: uint32;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadFixed32(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedFixed64(Stream: TProtoStreamReader; Data: TUInt64s): boolean;
var
  Len: uint32;
  NewDatum: uint64;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadFixed64(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedSFixed32(Stream: TProtoStreamReader; Data: TInt32s): boolean;
var
  Len: uint32;
  NewDatum: int32;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadSFixed32(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedSFixed64(Stream: TProtoStreamReader; Data: TInt64s): boolean;
var
  Len: uint32;
  NewDatum: int64;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadSFixed64(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedString(Stream: TProtoStreamReader; Data: TAnsiStrings): boolean;
var
  NewDatum: ansistring;
begin
  NewDatum := LoadString(Stream);
  Data.Add(NewDatum);

  Result := True;
end;

function LoadRepeatedBool(Stream: TProtoStreamReader; Data: TBooleans): boolean;
var
  Len: uint32;
  NewDatum: boolean;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadBool(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedByte(Stream: TProtoStreamReader; Data: TBytes): boolean;
var
  Len: uint32;
  NewDatum: byte;
  StartPos: integer;
begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := LoadByte(Stream);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

procedure SaveMessage(Stream: TProtoStreamWriter; const Data: TBaseMessage;
  const TagID: integer);
var
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, WIRETYPE_LENGTH_DELIMITED);
  SizeNode := Stream.AddIntervalNode;
  Data.SaveToStream(Stream);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

function LoadMessage(Stream: TProtoStreamReader; Data: TBaseMessage): boolean;
var
  Len: integer;
begin
  if Data = nil then
  begin
    WriteLn('Data Cannot be nil');
    Halt(1);
  end;

  Len := Stream.ReadVarUInt32;
  Result := Data.LoadFromStream(Stream, Len);

end;

generic procedure SaveRepeatedMessage<TMessage>(Stream: TProtoStreamWriter;
  const Data: specialize TObjectList<TMessage>; const TagID: integer);
var
  SizeNode: TLinkListNode;
  Message: TMessage;
begin
  if Data = nil then
    Exit;

  for Message in Data do
  begin
    SaveMessage(Stream, Message, TagID);

  end;
end;

generic function LoadRepeatedMessage<TMessage>(Stream: TProtoStreamReader;
  Data: specialize TObjectList<TMessage>): boolean;
var
  Len: uint32;
  NewDatum: TMessage;
  StartPos: integer;
begin
  NewDatum := TMessage.Create;
  Result := LoadMessage(Stream, NewDatum);
  Data.Add(NewDatum);
end;

procedure MaybeDispose(P: PDouble);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PSingle);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PInt16);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PInt32);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PInt64);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PUInt16);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PUInt32);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PUInt64);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PBoolean);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PAnsiString);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: pbyte);
begin
  if P <> nil then
    Dispose(P);

end;

procedure SaveRepeatedFloat(Stream: TProtoStreamWriter; const Data: TSingles;
  const TagID: integer);
var
  SingleData: single;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawData(@SingleData, SizeOf(single));

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedDouble(Stream: TProtoStreamWriter; const Data: TDoubles;
  const TagID: integer);
var
  SingleData: double;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawData(@SingleData, SizeOf(double));

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedInt32(Stream: TProtoStreamWriter; const Data: TInt32s;
  const TagID: integer);
var
  SingleData: int32;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawVarint32(SingleData);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedInt64(Stream: TProtoStreamWriter; const Data: TInt64s;
  const TagID: integer);
var
  SingleData: int64;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawVarint64(SingleData);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter; const Data: TUInt32s;
  const TagID: integer);
var
  SingleData: uint32;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawVarint32(SingleData);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter; const Data: TUInt64s;
  const TagID: integer);
var
  SingleData: uint64;
  SizeNode: TLinkListNode;
begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawVarint64(SingleData);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveFloat(Stream: TProtoStreamWriter; const Data: single;
  const TagID: integer);
const
  AlmostZero: double = 1e-10;
begin
  if (TagID = -1) or (AlmostZero < Abs(Data)) then
    Stream.WriteFloat(TagID, Data);

end;

procedure SaveDouble(Stream: TProtoStreamWriter; const Data: double;
  const TagID: integer);
const
  AlmostZero: double = 1e-10;
begin
  if (TagID = -1) or (AlmostZero < Abs(Data)) then
    Stream.WriteDouble(TagID, Data);

end;

procedure SaveInt32(Stream: TProtoStreamWriter; const Data: int32;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteVarInt32(TagID, Data);

end;

procedure SaveInt64(Stream: TProtoStreamWriter; const Data: int64;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteVarInt64(TagID, Data);

end;

procedure SaveUInt32(Stream: TProtoStreamWriter; const Data: uint32;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteVarUInt32(TagID, Data);

end;

procedure SaveUInt64(Stream: TProtoStreamWriter; const Data: uint64;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteVarUInt64(TagID, Data);

end;

const
  OneShl31 = 1 shl 31;

procedure SaveSInt32(Stream: TProtoStreamWriter; const Data: int32;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
  begin
    if Data and OneShl31 <> 0 then // Data < 0
    begin
      Stream.WriteVaruInt32(TagID, ((not Data) shl 1) or 1);
    end
    else
    begin
      Stream.WriteVaruInt32(TagID, Data shl 1);
    end;

  end;

end;

const
  OneShl63 = 1 shl 63;

procedure SaveSInt64(Stream: TProtoStreamWriter; const Data: int64;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
  begin
    if Data and OneShl63 <> 0 then // Data < 0
    begin
      Stream.WriteVarUInt64(TagID, ((not Data) shl 1) or 1);
    end
    else
    begin
      Stream.WriteVarUInt64(TagID, Data shl 1);
    end;

  end;
end;

procedure SaveFixed32(Stream: TProtoStreamWriter; const Data: uint32;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteFixed32(TagID, Data);

end;

procedure SaveFixed64(Stream: TProtoStreamWriter; const Data: uint64;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteFixed64(TagID, Data);

end;

procedure SaveSFixed32(Stream: TProtoStreamWriter; const Data: int32;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteSFixed32(TagID, Data);

end;

procedure SaveSFixed64(Stream: TProtoStreamWriter; const Data: int64;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteSFixed64(TagID, Data);

end;

procedure SaveString(Stream: TProtoStreamWriter; const Data: ansistring;
  const TagID: integer);
begin
  if (TagID = -1) or (Data <> '') then
    Stream.WriteString(TagID, Data);

end;

procedure SaveBool(Stream: TProtoStreamWriter; const Data: boolean;
  const TagID: integer);
begin
  if (TagID = -1) or Data then
    Stream.WriteBoolean(TagID, Data);

end;

procedure SaveByte(Stream: TProtoStreamWriter; const Data: byte; const TagID: integer);
begin
  if (TagID = -1) or (Data <> 0) then
    Stream.WriteByte(TagID, Data);

end;

function LoadFloat(Stream: TProtoStreamReader): single;
begin
  Result := Stream.ReadFloat;

end;

function LoadDouble(Stream: TProtoStreamReader): double;
begin
  Result := Stream.ReadDouble;
end;

function LoadInt32(Stream: TProtoStreamReader): int32;
begin
  Result := Stream.ReadVarInt32;

end;

function LoadInt64(Stream: TProtoStreamReader): int64;
begin
  Result := Stream.ReadVarInt64;

end;

function LoadUInt32(Stream: TProtoStreamReader): uint32;
begin
  Result := Stream.ReadVarUInt32;

end;

function LoadUInt64(Stream: TProtoStreamReader): uint64;
begin
  Result := Stream.ReadVarUInt64;
end;

function LoadSInt32(Stream: TProtoStreamReader): int32;
var
  Tmp: uint32;
begin
  Tmp := Stream.ReadVarUInt32;
  if Odd(Tmp) then
  begin
    Result := not ((Tmp xor 1) shr 1);

  end
  else
  begin
    Result := Tmp shr 1;

  end;

end;

function LoadSInt64(Stream: TProtoStreamReader): int64;
var
  Tmp: uint64;
begin
  Tmp := Stream.ReadVarUInt64;
  if Odd(Tmp) then
  begin
    Result := not ((Tmp xor 1) shr 1);

  end
  else
  begin
    Result := Tmp shr 1;

  end;

end;

function LoadFixed32(Stream: TProtoStreamReader): uint32;
begin
  Result := Stream.ReadFixed32;

end;

function LoadFixed64(Stream: TProtoStreamReader): uint64;
begin
  Result := Stream.ReadFixed64;

end;

function LoadSFixed32(Stream: TProtoStreamReader): int32;
begin
  Result := Stream.ReadSFixed32;

end;

function LoadSFixed64(Stream: TProtoStreamReader): int64;
begin
  Result := Stream.ReadSFixed64;

end;

function LoadString(Stream: TProtoStreamReader): ansistring;
begin
  Result := Stream.ReadString;

end;

function LoadBool(Stream: TProtoStreamReader): boolean;
begin
  Result := Stream.ReadBool;

end;

function LoadByte(Stream: TProtoStreamReader): byte;
begin
  Result := Stream.ReadByte;

end;

function SkipField(Stream: TProtoStreamReader; WireType: integer): boolean;
var
  Len: uint32;
begin
  Result := True;
  case WireType of
    0: // WireType 0: Varint (int32, int64, uint32, uint64, sint32, sint64, bool, enum)
    begin
      // Reading it as a 64-bit varint safely consumes all its bytes from the stream
      Stream.ReadVarUInt64;
    end;

    1: // WireType 1: 64-bit (fixed64, sfixed64, double)
    begin
      Stream.Position := Stream.Position + 8;
    end;

    2: // WireType 2: Length-delimited (string, bytes, embedded messages, packed repeated fields)
    begin
      Len := Stream.ReadVarUInt32;
      Stream.Position := Stream.Position + Len;
    end;

    5: // WireType 5: 32-bit (fixed32, sfixed32, float)
    begin
      Stream.Position := Stream.Position + 4;
    end;

    3, 4: // Groups (Deprecated in proto3)
    begin
      // You should generally not encounter these in proto3.
      Result := False;
    end;
    else
      Result := False; // Unknown wire type

  end;

end;

constructor TBaseMessage.Create;
begin
  inherited Create;

end;

destructor TBaseMessage.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseMessage.Clear;
begin

end;

function TBaseMessage.LoadFromStream(Stream: TStream): boolean;
var
  ProtoStream: TProtoStreamReader;
begin
  Self.Clear;
  ProtoStream := TProtoStreamReader.Create(Stream, False);

  Result := Self.LoadFromStream(ProtoStream, Stream.Size);

  ProtoStream.Free;
end;

procedure TBaseMessage.SaveToStream(Stream: TStream);
var
  ProtoStream: TProtoStreamWriter;
begin
  ProtoStream := TProtoStreamWriter.Create(Stream, False);

  Self.SaveToStream(ProtoStream);

  ProtoStream.Free;
end;

function TBaseMessage.ToJSON: ansistring;
var
  Streamer: TJSONStreamer;
begin
  if self = nil then
    Exit('{}');

  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options := Streamer.Options + [jsoTStringsAsArray];

  Result := Streamer.ObjectToJSONString(Self);

  Streamer.Free;

end;

end.
