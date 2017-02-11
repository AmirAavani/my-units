unit GenericStackUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, GenericCollectionUnit;

type

  { EStackIsEmpty }

  EStackIsEmpty= class(Exception)
    constructor Create;
  end;
  { TGenericAbstactStack }

  generic TGenericAbstactStack<TData>= class(TObject)
  end;
  { TGenericStack }

  {This stack is not suitable for primary data-types}
  generic TGenericStack<TData>= class(TStack)
  private
    function GetIsEmpty: Boolean;
    function GetTop: TData;
  public
    property IsEmpty: Boolean read GetIsEmpty;
    // property Item[Index: Integer]: TData read GetItem;
    property Top: TData read GetTop;

    function Find(AValue: TData): Boolean;
    function Pop: TData;
    procedure Clear;

    constructor Create;
    {
    TGenericStack does not free the members stored in it.
    }
    destructor Destroy; override;

  end;

    { TGenericStack }

  { TGenericStackForBuildInData }

  generic TGenericStackForBuildInData<TData>= class(specialize TGenericAbstactStack<TData>)
  private type
    TDataCollection= specialize TGenericCollectionForBuiltInData<TData>;

  private
    Elements: TDataCollection;

    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetTop: TData;
  public
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Top: TData read GetTop;

    function Pop: TData;
    procedure Push(Data: Tdata);

    constructor Create;
    {
    TGenericStack does not free the members stored in it.
    }
    destructor Destroy; override;

    procedure Clear;

  end;

implementation

{ EStackIsEmpty }

constructor EStackIsEmpty.Create;
begin
  inherited Create('Stack is Empty');

end;

{ TGenericStackForBuildInData }

function TGenericStackForBuildInData.GetCount: Integer;
begin
  Result:= Top;

end;

function TGenericStackForBuildInData.GetIsEmpty: Boolean;
begin
  Result:=(Count= 0);

end;

function TGenericStackForBuildInData.GetTop: TData;
begin
  Result:= Elements.Item [Count- 1];

end;

function TGenericStackForBuildInData.Pop: TData;
begin
  if IsEmpty then
    raise EStackIsEmpty.Create;

  Result:= GetTop;
  Elements.Delete(Count- 1);

end;

procedure TGenericStackForBuildInData.Push(Data: Tdata);
begin
  Elements.AddItem(Data);

end;

constructor TGenericStackForBuildInData.Create;
begin
  inherited Create;

  Elements:= TDataCollection.Create;

end;

destructor TGenericStackForBuildInData.Destroy;
begin
  Elements.Free;

  inherited Destroy;

end;

procedure TGenericStackForBuildInData.Clear;
begin
  Elements.Clear;

end;

{ TGenericStack }

function TGenericStack.GetIsEmpty: Boolean;
begin
  Result:=(Count= 0);

end;
                       {
function TGenericStack.GetItem(Index: Integer): TData;
begin
  Result := nil;//TData(Self.List.Items[Index]);
end;                   }

function TGenericStack.GetTop: TData;
begin
  Result:= TData(Peek);

end;

function TGenericStack.Pop: TData;
begin
  Result:= TData(inherited Pop);

end;

procedure TGenericStack.Clear;
begin
  while not IsEmpty do
    Self.Pop;
end;

constructor TGenericStack.Create;
begin
  inherited Create;

end;

destructor TGenericStack.Destroy;
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    Self.Pop.Free;

  inherited Destroy;
end;

function TGenericStack.Find(AValue: TData): Boolean;
var
  i: Integer;

begin
  Result := True;
  for i := 0 to Count - 1 do
    if TData(List.Items[i]) = AValue then
      Exit;

  Result := False;
end;

end.

