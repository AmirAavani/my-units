unit GenericCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, GenericCollection.UtilsUnit;

type

  { TCollection }

  generic TCollection<TData> = class(specialize TList<TData>)
  public type
    TDumpFunc = function (d: TData): TBytes;

  public
    procedure AddAnotherCollection(AnotherCollection: TCollection);
    procedure SaveToStream(Stream: TStream; DumpFunc: TDumpFunc);

  end;

  { TGenericCollection }

  generic TObjectCollection<TData>= class(specialize TList<TData>)
  private
    type
      TListData = specialize TList<TData>;

  public
    constructor Create(aList: TListData);
    constructor Create;
    destructor Destroy; override;

    {
      Deletes the Index-th item from the list and return it.
    }
    function Delete(Index: Integer): TData;
  end;

  { TMap }

  generic TMap<TKey, TValue> = class(specialize TAVLTreeMap<TKey, TValue>)
  public
    function Find(const aKey: TKey): TValue;
    function TryGetData(const AKey: TKey; out AData: TValue): Boolean;

  end;

  { TMapSimpleKeyObjectValue }

  generic TMapSimpleKeyObjectValue<TKey, TValue> = class(specialize TMap<TKey, TValue>)
  public
    destructor Destroy; override;

  end;

implementation

uses
  Generics.Defaults;

{ TCollection }

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
  bs: TBytes;

begin
  it := Self.GetEnumerator;

  while it.MoveNext do
  begin
    bs := DumpFunc(it.Current);
    Stream.WriteBuffer(bs, Length(bs));

  end;

  it.Free;
end;


{ TMap }

function TMap.Find(const aKey: TKey): TValue;
var
  pn: PNode;

begin
  Result := Default(TValue);

  pn := inherited Find(aKey);
  if pn <> nil then
    Result := pn^.Value;

end;

function TMap.TryGetData(const AKey: TKey; out AData: TValue): Boolean;
var
  pn: PNode;

begin
  pn := inherited Find(AKey);

  if pn = nil then
  begin
    AData := Default(TValue);
    Exit(False);

  end;
  Result := True;
  AData := pn^.Value;

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

constructor TObjectCollection.Create;
begin
  inherited Create;

end;

destructor TObjectCollection.Destroy;
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    TData(Self[i]).Free;

  inherited Destroy;

end;

function TObjectCollection.Delete(Index: Integer): TData;
begin
  Result := Self[Index];
  Self.Delete(Index);

end;

end.

