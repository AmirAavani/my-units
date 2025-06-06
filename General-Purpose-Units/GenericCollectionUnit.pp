unit GenericCollectionUnit;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections, TupleUnit;
type

  { TFixedSizeList }

  generic TFixedSizeList<TData> = class(TObject)
  public type
    PT = ^TData;

  private
    function GetItemsPtr: PT;
  protected
    FItems: array of TData;

  public
    property ItemsPtr: PT read GetItemsPtr;

    constructor Create(InitSize: Integer);

  end;

  { TCollection }

  generic TCollection<TData> = class(specialize TList<TData>)
  public type
    TLongIntDataPair = specialize TPair<LongInt, TData>;
    TBoolDataPair = specialize TPair<Boolean, TData>;
    TDumpFunc = function (constref d: TData; Stream: TStream): Boolean;
    TLoadFromStreamFunc = function (Stream: TStream): TData;
    // Loads the data from bp to bp + len - 1.
    // The result.First will be False if the interval contains partial content of
    // TData.
    // This function will be called again with the same "bp" value and a larger Len.
    TLoadFromBytesFunc = function (var bp: PByte; Len: Int32): TBoolDataPair;
    TMatcherFunc = function (constref Str: TData): Boolean;
    TPointerEnumerator = specialize TEnumerator<PT>;

  protected
    function GetItemsPtr: PT; inline;
    function GetIsEmpty: Boolean; inline;

  public
    property ItemsPtr: PT read GetItemsPtr;
    property IsEmpty: Boolean read GetIsEmpty;

    procedure AddAnotherCollection(AnotherCollection: TCollection);
    procedure SaveToStream(Stream: TStream; DumpFunc: TDumpFunc);

    class function LoadFromStream(
      Stream: TStream;
      LoadFunc: TLoadFromStreamFunc): specialize
      TCollection<TData>;
    class function BatchLoadFromStream(
      Stream: TStream;
      LoadFunc: TLoadFromBytesFunc;
      ChunkSize: UInt32 = 64 * 1024 * 1024): specialize
      TCollection<TData>;

    function Pop(n: Integer = 1): TData; virtual;
    procedure RemoveAllValuesMatching(Matcher: TMatcherFunc);
    function GetPtrEnumerator: TPointerEnumerator; override;


  end;

  { TMap }

  generic TMap<TKey, TValue> = class(specialize TAVLTreeMap<TKey, TValue>)
  public
    function Exists(constref Key: TKey): Boolean;
    function Find(constref Key: TKey): TValue;
    function Delete(constref Key: TKey; DisposeValue: Boolean): Boolean;
    function TryGetData(constref Key: TKey; out Data: TValue): Boolean;
    // Returns True if the key exists.
    function AddOrUpdateData(constref Key: TKey; Data: Tvalue): Boolean;

  end;

  { TMapSimpleKeyObjectValue }

  generic TMapSimpleKeyObjectValue<TKey, TValue> = class(specialize TMap<TKey, TValue>)
  public
    destructor Destroy; override;

  end;

implementation

uses
  Generics.Defaults, ALoggerUnit;

{ TFixedSizeList }

function TFixedSizeList.GetItemsPtr: PT;
begin
  Result := FItems[0];

end;

constructor TFixedSizeList.Create(InitSize: Integer);
begin
  inherited Create;

  SetLength(FItems, InitSize);

end;

{ TCollection }

function TCollection.GetItemsPtr: PT;
begin
  Result := @(Self.FItems[0]);
end;

function TCollection.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TCollection.AddAnotherCollection(AnotherCollection: TCollection);
var
  i: Integer;

begin
  if Self.Capacity < Self.Count + AnotherCollection.Count then
    Self.Capacity := Self.Count + AnotherCollection.Count;

  for i := 0 to AnotherCollection.Count - 1 do
    Self.Add(TData(AnotherCollection[i]));

end;

procedure TCollection.SaveToStream(Stream: TStream; DumpFunc: TDumpFunc);
var
  it: TCollection.TEnumerator;

begin
  it := Self.GetEnumerator;
  Stream.WriteDWord(Self.Count);

  while it.MoveNext do
  begin
    if not DumpFunc(it.Current, Stream) then
      FmtFatalLnIFFalse(False, 'Something went wrong', []);

  end;

  it.Free;
end;

class function TCollection.LoadFromStream(Stream: TStream; LoadFunc: TLoadFromStreamFunc
  ): specialize TCollection<TData>;
var
  x: TData;
  i: Integer;
  pItem: PT;

begin
  Result := (specialize TCollection<TData>).Create;
  Result.Count := Stream.ReadDWord;
  pItem := @Result.FItems[0];

  i := 0;
  while i < Result.Count do
  begin
    x := LoadFunc(Stream);
    pItem^ := x;
    Inc(pItem);
    Inc(i);

  end;
end;

class function TCollection.BatchLoadFromStream(
  Stream: TStream;
  LoadFunc: TLoadFromBytesFunc;
  ChunkSize: UInt32): specialize TCollection<TData>;

  function Reload(Current: PByte; var bs: array of Byte; Remaining: LongInt): LongInt;
  begin
    System.Move(Current, bs[0], Remaining);
    Result := Remaining;
    Stream.Read(bs[Remaining], ChunkSize - Remaining);

  end;
var
  x: TCollection.TBoolDataPair;
  Index: Integer;
  bs: array of Byte;
  bp: PByte;
  Remaining: Integer;

begin
  Result := (specialize TCollection<TData>).Create;
  Result.Count := Stream.ReadDWord;
  SetLength(bs , ChunkSize);
  Remaining := Stream.Read(bs, ChunkSize);

  bp := @bs[0];
  Index := 0;
  while Index < Result.Count do
  begin
    x := LoadFunc(bp, Remaining);
    if not x.First then
    begin
      Remaining := Reload(bp, bs, Remaining);


    end;
    Result[Index] := x.Second;
    Inc(Index);

  end;

  SetLength(bs, 0);

end;

function TCollection.Pop(n: Integer): TData;
begin
  if Count < n then
  begin
    FmtFatalLnIFFalse(False, 'Count = %d n: %d', [Count, n]);

  end;

  Result := Self[Count - n];
  while 0 < n do
  begin
    Self.Delete(Count - 1);
    Dec(n);

  end;

end;

procedure TCollection.RemoveAllValuesMatching(Matcher: TMatcherFunc);
var
  LastIndex: Integer;
  Str: TData;
  it: TCollection.TEnumerator;

begin
  LastIndex := 0;

  it := Self.GetEnumerator;
  while it.MoveNext do
  begin
    Str := it.Current;
    if not Matcher(Str) then
    begin
      Self[LastIndex] := Str;
      Inc(LastIndex);

    end;
  end;
  it.Free;

  Self.Count := LastIndex;

end;

function TCollection.GetPtrEnumerator: TPointerEnumerator;
begin
  Result := inherited GetPtrEnumerator();
end;

{
function TCollection.GetPtrEnumerator: TPointersEnumerator;
begin
  Result := inherited GetPtrEnumerator();
end;
}
{ TMap }

function TMap.Exists(constref Key: TKey): Boolean;
var
  pn: PNode;

begin
  pn := inherited Find(Key);
  Result := pn <> nil;

end;

function TMap.Find(constref Key: TKey): TValue;
var
  pn: PNode;

begin
  Result := Default(TValue);

  pn := inherited Find(Key);
  if pn <> nil then
    Result := pn^.Value;

end;

function TMap.Delete(constref Key: TKey; DisposeValue: Boolean): Boolean;
var
  Node: PNode;

begin
  Node := inherited Find(Key);

  if Node = nil then
    Exit(False);

  Result := True;
  inherited Delete(Node, DisposeValue);

end;

function TMap.TryGetData(constref Key: TKey; out Data: TValue): Boolean;
var
  pn: PNode;

begin
  pn := inherited Find(Key);

  if pn = nil then
  begin
    Data := Default(TValue);
    Exit(False);

  end;

  Result := True;
  Data := pn^.Value;

end;

function TMap.AddOrUpdateData(constref Key: TKey; Data: Tvalue): Boolean;
begin
  Result := Exists(key);

  if Result then
  begin
    Self[Key] := Data

  end
  else
  begin
    Self.Add(Key, Data);

  end;

end;

{ TMapSimpleKeyObjectValue }

destructor TMapSimpleKeyObjectValue.Destroy;
var
  it: TMapSimpleKeyObjectValue.TPairEnumerator;

begin
  it := Self.GetEnumerator;

  while it.MoveNext do
  begin
    it.Current.Value.Free;

  end;

  it.Free;

  inherited Destroy;
end;

end.

