unit HeapUnit;
{
  Tested with mowlawn @ USACO OPEN11 GOLD.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  { THeap }

  // MinHeap
  generic THeap<T> = class(TObject)
  private
    function Get(Index: Integer): T; inline;

  private
    property Member[Index: Integer]: T read Get;
    function GetCapacity: Integer;
    function GetCount: Integer; inline;
    function GetMin: T; inline;

    procedure Heapify(Index: Integer);
    function GetLeftChild(Index: Integer): Integer; inline;
    function GetRightChild(Index: Integer): Integer; inline;
    function GetParent(Index: Integer): Integer; inline;

  type
    TListOfT = specialize TList<T>;
  var
    FMembers: TListOfT;

  public
    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity;
    property Min: T read GetMin;

    // InitList must have the heap property
    // maybe by calling BuildHeap.
    constructor Create(InitLlist: TListOfT);

    procedure Insert(Data: T);
    procedure DeleteMin;

    procedure Print;


  end;

generic procedure BuildHeap<T>(List: specialize TList<T>);
generic procedure PrintHeap<T>(List: specialize TList<T>);
generic procedure Heapify<T>(List: specialize TList<T>; Index, n: Integer);
generic procedure DeleteMin<T>(List: specialize TList<T>);
generic procedure InsertValue<T>(List: specialize TList<T>; const v: T);

implementation

generic procedure Heapify<T>(List: specialize TList<T>; Index, n: Integer);
var
  l, r, Largest: Integer;
  Temp: T;

begin
  Largest := Index;
  l := Index shl 1;
  Inc(l);
  r := l + 1;

  if (l < n) and (List[Largest] < List[l]) then
    Largest := l;

  if (r < n) and (List[Largest] < List[r]) then
    Largest := r;

  if Largest <> Index then
  begin
    Temp := List[Index];
    List[Index] := List[Largest];
    List[Largest] := Temp;
    specialize Heapify<T>(List, Largest, n);

  end;
end;

generic procedure DeleteMin<T>(List: specialize TList<T>);
var
  LastElement: T;
  n: Integer;

begin
  n := List.Count;
  LastElement := List[n - 1];
  List[0] := LastElement;
  Dec(n);
  specialize Heapify<T>(List, 0, n);

end;

generic procedure InsertValue<T>(List: specialize TList<T>; const v: T);
var
  n: Integer;

begin
  List.Add(v);
  n := List.Count;
  specialize Heapify<T>(List, n - 1, n);

end;


generic procedure BuildHeap<T>(List: specialize TList<T>);
var
  n: SizeInt;

var
  i: Integer;

begin
  n := List.Count;
  for i := n shr 1 - 1 downto 0 do
    specialize Heapify<T>(List, i, n);

end;

generic procedure PrintHeap<T>(List: specialize TList<T>);
var
  Index, j, Two2Level: Integer;

begin
  WriteLn('----');
  Two2Level := 1;
  Index := 0;

  while Index < List.Count do
  begin
    for j := 1 to Two2Level do
    begin
      Write(List[Index], ', ');
      Inc(Index);

      if List.Count = Index then
        Break;

    end;
    WriteLn;
    Two2Level += Two2Level;

  end;

end;


{ THeap }

function THeap.Get(Index: Integer): T;
begin
  Result := FMembers[Index];

end;

function THeap.GetCapacity: Integer;
begin
  Result := FMembers.Capacity;
end;

function THeap.GetCount: Integer;
begin
  Result := FMembers.Count;

end;

function THeap.GetMin: T;
begin
  Result := Member[0];

end;

procedure THeap.Heapify(Index: Integer);
var
  ActiveIndex: Integer;
  MinOfChildrenIndex: Integer;
  MinOfChildren: T;
  Temp: T;

begin
  ActiveIndex := Index;

  while 2 * ActiveIndex + 1 < Count do
  begin
    MinOfChildrenIndex := 2 * ActiveIndex + 1;
    MinOfChildren := Member[MinOfChildrenIndex];

    if 2 * ActiveIndex + 2 < Count then
      if Member[2 * ActiveIndex + 2] < MinOfChildren then
      begin
        Inc(MinOfChildrenIndex);
        MinOfChildren := Member[MinOfChildrenIndex];

      end;

    if MinOfChildren < Member[ActiveIndex] then
    begin
      Temp := Member[ActiveIndex];
      FMembers[ActiveIndex] := MinOfChildren;
      FMembers[MinOfChildrenIndex] := Temp;

      ActiveIndex := MinOfChildrenIndex;

    end
    else
      Break;

  end;

end;

function THeap.GetLeftChild(Index: Integer): Integer;
begin
  Result := Index shl 1 + 1;

end;

function THeap.GetRightChild(Index: Integer): Integer;
begin
  Result := Index shl 1 + 2;

end;

function THeap.GetParent(Index: Integer): Integer;
begin
  Result := (Index - 1) shr 1;

end;

constructor THeap.Create(InitLlist: TListOfT);
begin
  inherited Create;

  FMembers := InitLlist;
end;

procedure THeap.Insert(Data: T);
var
  ActiveIndex: Integer;
  Temp: T;
begin
  FMembers.Add(Data);

  ActiveIndex := Count - 1;
  if Count = 1 then
    Exit;

  while Member[ActiveIndex] < Member[GetParent(ActiveIndex)] do
  begin
    Temp := Member[ActiveIndex];
    FMembers[ActiveIndex] := FMembers[GetParent(ActiveIndex)];
    FMembers[GetParent(ActiveIndex)] := Temp;

    ActiveIndex := GetParent(ActiveIndex);
    if ActiveIndex = 0 then
      Break;

  end;

end;

procedure THeap.DeleteMin;
begin
  FMembers[0] := FMembers[Count - 1];

  Heapify(0);
  FMembers.Count := Count - 1;

end;

procedure THeap.Print;
var
  Index, j, Two2Level: Integer;

begin
  WriteLn('----');
  Two2Level := 1;
  Index := 0;

  while Index < FMembers.Count do
  begin
    for j := 1 to Two2Level do
    begin
      Write(FMembers[Index].ToString, ', ');
      Inc(Index);

      if FMembers.Count = Index then
        Break;

    end;
    WriteLn;
    Two2Level += Two2Level;

  end;

end;

end.
