unit GenericCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  { TGenericCollection }

  generic TObjectCollection<TData>= class(specialize TList<TData>)
  private
    type
      TListData = specialize TList<TData>;

  public
    constructor Create(aList: TListData);
    constructor Create;
    destructor Destroy; override;

    procedure AddAnotherCollection(AnotherCollection: TObjectCollection);

    {
      Deletes the Index-th item from the list and return it.
    }
    function Delete(Index: Integer): TData;
  end;

  generic TMap<Key, Value> = class(specialize TAVLTreeMap<Key, Value>)
  end;

  { TMapSimpleKeyObjectValue }

  generic TMapSimpleKeyObjectValue<Key, Value> = class(specialize TMap<Key, Value>)
  public
    destructor Destroy; override;

  end;

implementation

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

procedure TObjectCollection.AddAnotherCollection(AnotherCollection: TObjectCollection);
var
  i: Integer;

begin
  for i := 0 to AnotherCollection.Count- 1 do
    Self.Add(TData(AnotherCollection[i]));

end;

function TObjectCollection.Delete(Index: Integer): TData;
begin
  Result := Self[Index];
  Self.Delete(Index);

end;

end.

