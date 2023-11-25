unit GenericCollectionUnit;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections, GenericCollection.UtilsUnit;
type

  { TCollection }

  generic TCollection<TData> = class(specialize TList<TData>)
  private
    function GetIsEmpty: Boolean; inline;

  public type
    TDumpFunc = function (constref d: TData; Stream: TStream): Boolean;
    TLoadFunc = function (Stream: TStream): TData;
    TMatcherFunc = function (constref Str: TData): Boolean;
    TPointerEnumerator = specialize TEnumerator<PT>;

  public
    property IsEmpty: Boolean read GetIsEmpty;

    procedure AddAnotherCollection(AnotherCollection: TCollection);
    procedure SaveToStream(Stream: TStream; DumpFunc: TDumpFunc);

    class function LoadFromStream(Stream: TStream; LoadFunc: TLoadFunc): specialize
      TCollection<TData>;

    function Pop(n: Integer = 1): TData; virtual;
    procedure RemoveAllValuesMatching(Matcher: TMatcherFunc);
    function GetPtrEnumerator: TPointerEnumerator;


  end;

{  operator Enumerator(aEnum: specialize TEnumerator<TMyList.PT>): specialize TEnumerator<TMyList.PT>;
  begin
   Result := aEnum;
  end;
}
type
  { TObjectCollection }

  generic TObjectCollection<TData>= class(specialize TList<TData>)
  private
    type
      TListData = specialize TList<TData>;

  protected
    function GetCount: SizeInt; override;

  public
    property Count: SizeInt read GetCount write SetCount;

    constructor Create;
    constructor Create(aList: TListData);
    destructor Destroy; override;

    {
      Deletes the Index-th item from the list and return it.
    }
    function Delete(Index: SizeInt): TData;

    procedure AddAnotherCollection(Another: TListData);
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

{ TCollection }

function TCollection.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TCollection.AddAnotherCollection(AnotherCollection: TCollection);
var
  i: Integer;

begin
  for i := 0 to AnotherCollection.Count - 1 do
    Self.Add(TData(AnotherCollection[i]));

end;

procedure TCollection.SaveToStream(Stream: TStream; DumpFunc: TDumpFunc);
var
  it: TCollection.TEnumerator;

begin
  it := Self.GetEnumerator;

  while it.MoveNext do
  begin
    if not DumpFunc(it.Current, Stream) then
      FmtFatalLnIFFalse(False, 'Something went wrong', []);

  end;

  it.Free;
end;

class function TCollection.LoadFromStream(Stream: TStream; LoadFunc: TLoadFunc
  ): specialize TCollection<TData>;
var
  x: TData;

begin
  Result := (specialize TCollection<TData>).Create;

  while Stream.Position < Stream.Size do
  begin
    x := LoadFunc(Stream);
    Result.Add(x);

  end;
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

{ TGenericCollection }

constructor TObjectCollection.Create(aList: TListData);
var
  i: Integer;

begin
  inherited Create;

  Self.Count := aList.Count;
  for i := 0 to aList.Count - 1 do
    Self.Add(aList[i]);

end;

function TObjectCollection.GetCount: SizeInt;
begin
  if Self = nil then
    Exit(0);

  Result := Self.FLength;
end;

constructor TObjectCollection.Create;
begin
  inherited Create;

end;

destructor TObjectCollection.Destroy;
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    Self[i].Free;

  inherited Destroy;

end;

function TObjectCollection.Delete(Index: SizeInt): TData;
begin
  inherited Delete(Index);

end;

procedure TObjectCollection.AddAnotherCollection(Another: TListData);
var
  i: Integer;

begin
  for i := 0 to Another.Count - 1 do
    Self.Add(Another[i]);

end;

end.

