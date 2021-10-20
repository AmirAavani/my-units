unit CollectionUnit;
{$mode objfpc}

interface
uses
  Classes, SysUtils, GenericCollectionUnit;
  
type

  { TInt64Collection }

  TInt64Collection= class (specialize TCollection<Int64>)
  public

    procedure FillWithZero (Length: Integer);
    function Min: Int64;
    function Max: Int64;
    function Avg: Int64;

  end;

  { TIntegerCollection }

  TIntegerCollection= class (specialize TCollection<Integer>)
  public
    procedure FillWithZero (Length: Integer);

  end;

  TAnsiStrings = specialize TCollection<AnsiString>;
  TBooleans = specialize TCollection<Boolean>;

implementation

{ TIntegerCollection }

procedure TIntegerCollection.FillWithZero (Length: Integer);
var
  i: Integer;

begin
  Count := Length;
  for i := 0 to Count- 1 do
    Items[i] := 0;

end;

{ TInt64Collection }

function TInt64Collection.Avg: Int64;
var
  i: Integer;

begin
  Result := 0;

  for i := 0 to Count- 1 do
    Inc (Result, Self[i]);

  if Count <> 0 then
    Result := Result div Count;

end;

procedure TInt64Collection.FillWithZero (Length: Integer);
var
  i: Integer;

begin
  Count := Length;
  
  for i := 0 to Count- 1 do
    Items[i] := 0;

end;

function TInt64Collection.Max: Int64;
var
  i: Integer;

begin
  Result := 0;
  if 0 < Count then
    Result := Self[0];

  for i := 1 to Count- 1 do
    if Result < Self[i] then
      Result := self[i];

end;

function TInt64Collection.Min: Int64;
var
  i: Integer;

begin
  Result := 0;

  if 0 < Count then
    Result := Self[0];

  for i := 1 to Count- 1 do
    if Self[i] < Result then
      Result := Self[i];

end;

end.

