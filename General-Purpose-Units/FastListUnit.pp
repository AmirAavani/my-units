unit FastListUnit;

{$mode ObjFPC}{$H+}

interface

type
    // TFastList is a List with no range checking.
  generic TFastList<T> = class(TObject)
  public
  type
    PT = ^T; // Typed pointer for the generic type
  private
    FItems: array of T;
    FCount: integer;
    FCapacity: integer;
    procedure SetCapacity(NewCapacity: integer); inline;

    // Explicitly disable range checking for these specific getters/setters
    // just in case global $R+ is accidentally enabled in your project settings.
    {$PUSH}{$R-}
    function GetItem(Index: integer): T; inline;
    procedure SetItem(Index: integer; const Value: T); inline;
    {$POP}
  public
    constructor Create(InitialCapacity: integer = 64);
    destructor Destroy; override;

    procedure Add(const Item: T); inline;
    procedure Clear; inline;
    function GetPointer: PT; inline;

    property Count: integer read FCount;
    property Capacity: integer read FCapacity write SetCapacity;
    property Items[Index: integer]: T read GetItem write SetItem; default;
  end;

implementation

constructor TFastList.Create(InitialCapacity: integer = 64);
begin
  inherited Create;

  FCount := 0;
  FCapacity := 0;
  SetCapacity(InitialCapacity);
end;

destructor TFastList.Destroy;
begin
  SetLength(FItems, 0);

  inherited Destroy;
end;

procedure TFastList.SetCapacity(NewCapacity: integer);
begin
  if NewCapacity < FCount then
    NewCapacity := FCount;

  if NewCapacity <> FCapacity then
  begin
    SetLength(FItems, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TFastList.Add(const Item: T);
begin
  if FCount = FCapacity then
  begin
    if FCapacity = 0 then
      SetCapacity(64)
    else
      SetCapacity(FCapacity * 2); // Standard doubling strategy for O(1) amortized cost
  end;

  FItems[FCount] := Item;
  Inc(FCount);
end;

function TFastList.GetItem(Index: integer): T;
begin
  Result := FItems[Index];
end;

procedure TFastList.SetItem(Index: integer; const Value: T);
begin
  FItems[Index] := Value;
end;

procedure TFastList.Clear;
begin
  FCount := 0;
  // Note: We don't shrink capacity here to avoid reallocation overhead
  // on subsequent additions. Call SetCapacity(0) if memory freeing is strictly needed.
end;

function TFastList.GetPointer: PT;
begin
  if FCount = 0 then
    Result := nil
  else
    Result := @FItems[0];
end;

end.
