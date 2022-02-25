unit ProtoHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperListsUnit, ProtoStreamUnit;

type
  TBytes = specialize TSimpleTypeList<Byte>;
  TSingles = specialize TSimpleTypeList<Single>;
  TDoubles = specialize TSimpleTypeList<Double>;
  TInt32s = specialize TSimpleTypeList<Int32>;
  TInt64s = specialize TSimpleTypeList<Int64>;
  TUInt32s = specialize TSimpleTypeList<UInt32>;
  TUInt64s = specialize TSimpleTypeList<UInt64>;
  TBooleans = specialize TSimpleTypeList<Boolean>;
  TAnsiStrings = specialize TSimpleTypeList<AnsiString>;

  TBaseMessage = class;
  TBaseOneOf = class;

  { TBaseMessage }

  TBaseMessage = class(TObject)
  protected

    procedure SaveToStream(Stream: TProtoStreamWriter);  virtual; abstract;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;  virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;

    function LoadFromStream(Stream: TStream): Boolean; virtual;
    procedure SaveToStream(Stream: TStream); virtual;


    function ToJSON: AnsiString; virtual;

  end;

  { TBaseOneOf }

  TBaseOneOf = class(TObject)
  protected
    _ObjectIndex: Integer;
    _Data: Pointer;

    function GetPointerByIndex(Index: Integer): Pointer;
    procedure SetPointerByIndex(Index: Integer; AValue: Pointer);

  public
    property PointerByIndex[Index: Integer]: Pointer read GetPointerByIndex write SetPointerByIndex;

    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;

  end;

  { EBaseOneOf }

  EBaseOneOf = class(Exception)
  public
    constructor CreateTwoValueAreSet;
  end;

procedure SaveFloat(Stream: TProtoStreamWriter; const Data: Single;
  const TagID: Integer);
procedure SaveDouble(Stream: TProtoStreamWriter; const Data: Double;
  const TagID: Integer);
procedure SaveInt32(Stream: TProtoStreamWriter; const Data: Int32;
  const TagID: Integer);
procedure SaveInt64(Stream: TProtoStreamWriter; const Data: Int64;
  const TagID: Integer);
procedure SaveUInt32(Stream: TProtoStreamWriter; const Data: UInt32;
  const TagID: Integer);
procedure SaveUInt64(Stream: TProtoStreamWriter; const Data: UInt64;
  const TagID: Integer);
procedure SaveSInt32(Stream: TProtoStreamWriter; const Data: Int32; const TagID: Integer);
procedure SaveSInt64(Stream: TProtoStreamWriter;
    const Data: Int64; const TagID: Integer);
procedure SaveFixed32(Stream: TProtoStreamWriter;
    const Data: UInt32; const TagID: Integer);
procedure SaveFixed64(Stream: TProtoStreamWriter;
    const Data: UInt64; const TagID: Integer);
procedure SaveSFixed32(Stream: TProtoStreamWriter;
    const Data: Int32; const TagID: Integer);
procedure SaveSFixed64(Stream: TProtoStreamWriter;
    const Data: Int64; const TagID: Integer);
procedure SaveString(Stream: TProtoStreamWriter; const Data: AnsiString;
  const TagID: Integer);
procedure SaveBool(Stream: TProtoStreamWriter; const Data: Boolean;
    const TagID: Integer);
procedure SaveByte(Stream: TProtoStreamWriter; const Data: Byte;
    const TagID: Integer);

function LoadFloat(Stream: TProtoStreamReader): Single;
function LoadDouble(Stream: TProtoStreamReader): Double;
function LoadInt32(Stream: TProtoStreamReader): Int32;
function LoadInt64(Stream: TProtoStreamReader): Int64;
function LoadUInt32(Stream: TProtoStreamReader): UInt32;
function LoadUInt64(Stream: TProtoStreamReader): UInt64;
function LoadSInt32(Stream: TProtoStreamReader): Int32;
function LoadSInt64(Stream: TProtoStreamReader): Int64;
function LoadFixed32(Stream: TProtoStreamReader): UInt32;
function LoadFixed64(Stream: TProtoStreamReader): UInt64;
function LoadSFixed32(Stream: TProtoStreamReader): Int32;
function LoadSFixed64(Stream: TProtoStreamReader): Int64;
function LoadString(Stream: TProtoStreamReader): AnsiString;
function LoadBool(Stream: TProtoStreamReader): Boolean;
function LoadByte(Stream: TProtoStreamReader): Byte;

// TODO(Amir): Maybe replace this methods with a generic function.
procedure SaveRepeatedFloat(Stream: TProtoStreamWriter;
  const Data: TSingles;
  const TagID: Integer);
procedure SaveRepeatedDouble(Stream: TProtoStreamWriter;
    const Data: TDoubles;
    const TagID: Integer);
procedure SaveRepeatedInt32(Stream: TProtoStreamWriter;
    const Data: TInt32s;
    const TagID: Integer);
procedure SaveRepeatedInt64(Stream: TProtoStreamWriter;
    const Data: TInt64s;
    const TagID: Integer);
procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter;
    const Data: TUInt32s;
    const TagID: Integer);
procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter;
    const Data: TUInt64s;
    const TagID: Integer);
procedure SaveRepeatedSInt32(Stream: TProtoStreamWriter;
    const Data: TInt32s;
    const TagID: Integer);
procedure SaveRepeatedSInt64(Stream: TProtoStreamWriter;
    const Data: TInt64s;
    const TagID: Integer);
procedure SaveRepeatedFixed32(Stream: TProtoStreamWriter;
    const Data: TUInt32s;
    const TagID: Integer);
procedure SaveRepeatedFixed64(Stream: TProtoStreamWriter;
    const Data: TUInt64s;
    const TagID: Integer);
procedure SaveRepeatedSFixed32(Stream: TProtoStreamWriter;
    const Data: TInt32s;
    const TagID: Integer);
procedure SaveRepeatedSFixed64(Stream: TProtoStreamWriter;
    const Data: TInt64s;
    const TagID: Integer);
procedure SaveRepeatedString(
    Stream: TProtoStreamWriter;
   const Data: TAnsiStrings;
  const TagID: Integer);
procedure SaveRepeatedBool(Stream: TProtoStreamWriter;
    const Data: TBooleans;
    const TagID: Integer);
procedure SaveRepeatedByte(Stream: TProtoStreamWriter;
  const Data: TBytes;
  const TagID: Integer);

// TODO(Amir): Maybe replace this methods with a generic function.
function LoadRepeatedFloat(Stream: TProtoStreamReader;
    Data: TSingles): Boolean;
function LoadRepeatedDouble(Stream: TProtoStreamReader;
    Data: TDoubles): Boolean;
function LoadRepeatedInt32(Stream: TProtoStreamReader;
    Data: TInt32s): Boolean;
function LoadRepeatedInt64(Stream: TProtoStreamReader;
    Data: TInt64s): Boolean;
function LoadRepeatedUInt32(Stream: TProtoStreamReader;
    Data: TUInt32s): Boolean;
function LoadRepeatedUInt64(Stream: TProtoStreamReader;
    Data: TUInt64s): Boolean;
function LoadRepeatedSint32(Stream: TProtoStreamReader;
    Data: TInt32s): Boolean;
function LoadRepeatedSint64(Stream: TProtoStreamReader;
    Data: TInt64s): Boolean;
function LoadRepeatedFixed32(Stream: TProtoStreamReader;
    Data: TUInt32s): Boolean;
function LoadRepeatedFixed64(Stream: TProtoStreamReader;
    Data: TUInt64s): Boolean;
function LoadRepeatedSFixed32(Stream: TProtoStreamReader;
    Data: TInt32s): Boolean;
function LoadRepeatedSFixed64(Stream: TProtoStreamReader;
    Data: TInt64s): Boolean;
function LoadRepeatedString(Stream: TProtoStreamReader;
     Data: TAnsiStrings): Boolean;
function LoadRepeatedBool(Stream: TProtoStreamReader;
    Data: TBooleans): Boolean;
function LoadRepeatedByte(Stream: TProtoStreamReader;
    Data: TBytes): Boolean;

procedure SaveMessage(Stream: TProtoStreamWriter;
    const Data: TBaseMessage;
    const TagID: Integer);
function LoadMessage(Stream: TProtoStreamReader;
    const Data: TBaseMessage): Boolean;
generic procedure SaveRepeatedMessage<TMessage>(Stream: TProtoStreamWriter;
    const Data: specialize TObjectList<TMessage>;
    const TagID: Integer);
generic function LoadRepeatedMessage<TMessage>(Stream: TProtoStreamReader;
    Data: specialize TObjectList<TMessage>): Boolean;

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
procedure MaybeDispose(P: PByte);

implementation
uses
  fpjsonrtti;

{ EBaseOneOf }

constructor EBaseOneOf.CreateTwoValueAreSet;
begin
  inherited Create('Two values of an OneOf Field are set!');

end;

{ TBaseOneOf }

function TBaseOneOf.GetPointerByIndex(Index: Integer): Pointer;
begin
  if Self = nil then
    Exit(nil);

  if Index = _ObjectIndex then
    Exit(_Data);

  Result := nil;

end;

procedure TBaseOneOf.SetPointerByIndex(Index: Integer; AValue: Pointer);
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
  const TagID: Integer);
var
  SingleData: Int32;
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
  const TagID: Integer);
var
  SingleData: Int64;
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
  const TagID: Integer);
var
  SingleData: UInt32;
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
  const TagID: Integer);
var
  SingleData: UInt64;
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
  const TagID: Integer);
var
  SingleData: Int32;
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
  const TagID: Integer);
var
  SingleData: UInt64;
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

procedure SaveRepeatedString(Stream: TProtoStreamWriter;
  const Data: TAnsiStrings; const TagID: Integer);
var
  SingleData: AnsiString;

begin
  if Data = nil then
    Exit;

  for SingleData in Data do
  begin
    SaveString(Stream, SingleData, TagID );

  end;

end;

procedure SaveRepeatedBool(Stream: TProtoStreamWriter;
  const Data: TBooleans; const TagID: Integer);
var
  SingleData: Boolean;
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
  const TagID: Integer);
var
  SingleData: Byte;
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

function LoadRepeatedFloat(Stream: TProtoStreamReader;
  Data: TSingles): Boolean;
var
  Len: uInt32;
  NewDatum: Single;
  StartPos: Integer;

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

function LoadRepeatedDouble(Stream: TProtoStreamReader;
  Data: TDoubles): Boolean;
var
  Len: uInt32;
  NewDatum: Double;
  StartPos: Integer;

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

function LoadRepeatedInt32(Stream: TProtoStreamReader;
  Data: TInt32s): Boolean;
var
  Len: uInt32;
  NewDatum: Int32;
  StartPos: Integer;

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

function LoadRepeatedInt64(Stream: TProtoStreamReader;
  Data: TInt64s): Boolean;
var
  Len: uInt32;
  NewDatum: Int64;
  StartPos: Integer;

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

function LoadRepeatedUInt32(Stream: TProtoStreamReader;
  Data: TUInt32s): Boolean;
var
  Len: uInt32;
  NewDatum: UInt32;
  StartPos: Integer;

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

function LoadRepeatedUInt64(Stream: TProtoStreamReader;
  Data: TUInt64s): Boolean;
var
  Len: uInt32;
  NewDatum: UInt64;
  StartPos: Integer;

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

function LoadRepeatedSint32(Stream: TProtoStreamReader; Data: TInt32s): Boolean;
var
  Len: uInt32;
  NewDatum: Int32;
  StartPos: Integer;

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

function LoadRepeatedSint64(Stream: TProtoStreamReader; Data: TInt64s): Boolean;
var
  Len: uInt32;
  NewDatum: Int64;
  StartPos: Integer;

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

function LoadRepeatedFixed32(Stream: TProtoStreamReader; Data: TUInt32s
  ): Boolean;
var
  Len: UInt32;
  NewDatum: UInt32;
  StartPos: Integer;

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

function LoadRepeatedFixed64(Stream: TProtoStreamReader; Data: TUInt64s
  ): Boolean;
var
  Len: UInt32;
  NewDatum: UInt64;
  StartPos: Integer;

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

function LoadRepeatedSFixed32(Stream: TProtoStreamReader; Data: TInt32s
  ): Boolean;
var
  Len: UInt32;
  NewDatum: Int32;
  StartPos: Integer;

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

function LoadRepeatedSFixed64(Stream: TProtoStreamReader; Data: TInt64s
  ): Boolean;
var
  Len: UInt32;
  NewDatum: Int64;
  StartPos: Integer;

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

function LoadRepeatedString(Stream: TProtoStreamReader;
  Data: TAnsiStrings): Boolean;
var
  NewDatum: AnsiString;

begin
  NewDatum := LoadString(Stream);
  Data.Add(NewDatum);

  Result := True;
end;

function LoadRepeatedBool(Stream: TProtoStreamReader;
  Data: TBooleans): Boolean;
var
  Len: uInt32;
  NewDatum: Boolean;
  StartPos: Integer;

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

function LoadRepeatedByte(Stream: TProtoStreamReader; Data: TBytes): Boolean;
var
  Len: uInt32;
  NewDatum: Byte;
  StartPos: Integer;

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

procedure SaveMessage(Stream: TProtoStreamWriter;
  const Data: TBaseMessage; const TagID: Integer);
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

function LoadMessage(Stream: TProtoStreamReader;
  const Data: TBaseMessage): Boolean;
var
  Len: Integer;

begin
  Len := Stream.ReadVarUInt32;

  Result := Data.LoadFromStream(Stream, Len);

end;

generic procedure SaveRepeatedMessage<TMessage>(Stream: TProtoStreamWriter;
    const Data: specialize TObjectList<TMessage>;
    const TagID: Integer);
var
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
    Data: specialize TObjectList<TMessage>): Boolean;
var
  Len: uInt32;
  NewDatum: TMessage;
  StartPos: Integer;

begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := TMessage.Create;
    LoadMessage(Stream, NewDatum);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
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

procedure MaybeDispose(P: PByte);
begin
  if P <> nil then
    Dispose(P);

end;

procedure SaveRepeatedFloat(Stream: TProtoStreamWriter;
  const Data: TSingles; const TagID: Integer);
var
  SingleData: Single;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawData(@SingleData, SizeOf(Single));

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedDouble(Stream: TProtoStreamWriter;
  const Data: TDoubles; const TagID: Integer);
var
  SingleData: Double;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawData(@SingleData, SizeOf(Double));

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedInt32(Stream: TProtoStreamWriter;
  const Data: TInt32s; const TagID: Integer);
var
  SingleData: Int32;
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

procedure SaveRepeatedInt64(Stream: TProtoStreamWriter;
  const Data: TInt64s; const TagID: Integer);
var
  SingleData: Int64;
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

procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter;
  const Data: TUInt32s; const TagID: Integer);
var
  SingleData: UInt32;
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

procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter;
  const Data: TUInt64s; const TagID: Integer);
var
  SingleData: UInt64;
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

procedure SaveFloat(Stream: TProtoStreamWriter; const Data: Single;
  const TagID: Integer);
const
  AlmostZero: Double = 1e-10;

begin
  if (TagID = -1) or  (AlmostZero < Abs(Data)) then
    Stream.WriteFloat(TagID, Data);

end;

procedure SaveDouble(Stream: TProtoStreamWriter; const Data: Double;
  const TagID: Integer);
const
  AlmostZero: Double = 1e-10;

begin
  if (TagID = -1) or  (AlmostZero < Abs(Data)) then
    Stream.WriteDouble(TagID, Data);

end;

procedure SaveInt32(Stream: TProtoStreamWriter; const Data: Int32;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteVarInt32(TagID, Data);

end;

procedure SaveInt64(Stream: TProtoStreamWriter; const Data: Int64;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteVarInt64(TagID, Data);

end;

procedure SaveUInt32(Stream: TProtoStreamWriter; const Data: UInt32;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteVarUInt32(TagID, Data);

end;

procedure SaveUInt64(Stream: TProtoStreamWriter; const Data: UInt64;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteVarUInt64(TagID, Data);

end;

const
  OneShl31 = 1 shl 31;

procedure SaveSInt32(Stream: TProtoStreamWriter; const Data: Int32;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
  begin
    if Data and OneShl31 <> 0 then // Data < 0
    begin
      Stream.WriteVaruInt32(TagID, ((not Data) shl 1) or 1)
    end
    else
    begin
      Stream.WriteVaruInt32(TagID, Data shl 1);
    end;

  end;

end;

const
  OneShl63 = 1 shl 63;

procedure SaveSInt64(Stream: TProtoStreamWriter; const Data: Int64;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
  begin
    if Data and OneShl63 <> 0 then // Data < 0
    begin
      Stream.WriteVarUInt64(TagID, ((not Data) shl 1) or 1)
    end
    else
    begin
      Stream.WriteVarUInt64(TagID, Data shl 1);
    end;

  end;
end;

procedure SaveFixed32(Stream: TProtoStreamWriter; const Data: UInt32;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteFixed32(TagID, Data);

end;

procedure SaveFixed64(Stream: TProtoStreamWriter; const Data: UInt64;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteFixed64(TagID, Data);

end;

procedure SaveSFixed32(Stream: TProtoStreamWriter; const Data: Int32;
  const TagID: Integer);
begin
  if(TagID = -1) or  (Data <> 0) then
    Stream.WriteSFixed32(TagID, Data);

end;

procedure SaveSFixed64(Stream: TProtoStreamWriter; const Data: Int64;
  const TagID: Integer);
begin
  if(TagID = -1) or  (Data <> 0) then
    Stream.WriteSFixed64(TagID, Data);

end;

procedure SaveString(Stream: TProtoStreamWriter; const Data: AnsiString;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> '') then
    Stream.WriteString(TagID, Data);

end;

procedure SaveBool(Stream: TProtoStreamWriter; const Data: Boolean;
  const TagID: Integer);
begin
  if (TagID = -1) or  Data then
    Stream.WriteBoolean(TagID, Data);

end;

procedure SaveByte(Stream: TProtoStreamWriter; const Data: Byte;
  const TagID: Integer);
begin
  if (TagID = -1) or  (Data <> 0) then
    Stream.WriteByte(TagID, Data);

end;

function LoadFloat(Stream: TProtoStreamReader): Single;
begin
  Result := Stream.ReadFloat;

end;

function LoadDouble(Stream: TProtoStreamReader): Double;
begin
  Result := Stream.ReadDouble;
end;

function LoadInt32(Stream: TProtoStreamReader): Int32;
begin
  Result := Stream.ReadVarInt32;

end;

function LoadInt64(Stream: TProtoStreamReader): Int64;
begin
  Result := Stream.ReadVarInt64;

end;

function LoadUInt32(Stream: TProtoStreamReader): UInt32;
begin
  Result := Stream.ReadVarUInt32

end;

function LoadUInt64(Stream: TProtoStreamReader): UInt64;
begin    
  Result := Stream.ReadVarUInt64
end;

function LoadSInt32(Stream: TProtoStreamReader): Int32;
var
  Tmp: UInt32;

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

function LoadSInt64(Stream: TProtoStreamReader): Int64;
var
  Tmp: UInt64;

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

function LoadFixed32(Stream: TProtoStreamReader): UInt32;
begin
  Result := Stream.ReadFixed32;

end;

function LoadFixed64(Stream: TProtoStreamReader): UInt64;
begin
  Result := Stream.ReadFixed64;

end;

function LoadSFixed32(Stream: TProtoStreamReader): Int32;
begin
  Result := Stream.ReadSFixed32;

end;

function LoadSFixed64(Stream: TProtoStreamReader): Int64;
begin
  Result := Stream.ReadSFixed64;

end;

function LoadString(Stream: TProtoStreamReader): AnsiString;
begin
  Result := Stream.ReadString;

end;

function LoadBool(Stream: TProtoStreamReader): Boolean;
begin
  Result := Stream.ReadBool;

end;

function LoadByte(Stream: TProtoStreamReader): Byte;
begin
  Result := Stream.ReadByte;

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

function TBaseMessage.LoadFromStream(Stream: TStream): Boolean;
var
  ProtoStream: TProtoStreamReader;

begin
  Self.Clear;
  ProtoStream := TProtoStreamReader.Create(Stream);

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

function TBaseMessage.ToJSON: AnsiString;
var
  Streamer: TJSONStreamer;

begin
  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options := Streamer.Options + [jsoTStringsAsArray];

  Result := Streamer.ObjectToJSONString(Self);

  Streamer.Free;

end;

end.
