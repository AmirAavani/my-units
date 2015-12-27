unit SpanningTreeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HeapUnit;

type
  TAdjMat = array of array of Extended;
  TTreeEdge = record
    Node: Integer;
    Parent: Integer;
    Weight: Extended;
  end;
  TTree = array of TTreeEdge;

function MinWeightSpanningTree(const Mat: TAdjMat; VCount: Integer; var SpanningTree: TTree): Integer;

implementation
uses fgl, Math;

type

  { TPair }

  TPair = class(TObject)
    Node: Integer;
    Cost: Extended;
    ParentNode: Integer;
    constructor Create(n: Integer; c: Extended; p: Integer);
  end;
  TPairHeap = specialize THeap<TPair>;

function PairGreaterThan(const P1, P2: TPair): Boolean;
begin
  if P1.Node <> P2.Node then
    Result := P1.Node < P2.Node
  else
    Result := P1.Cost < P2.Cost
end;

function MinWeightSpanningTree(const Mat: TAdjMat; VCount: Integer; var SpanningTree: TTree): Integer;
const
  Inf = 1e100;
var
  Pair: TPair;
  Heap: TPairHeap;
  i: Integer;
  v: Integer;
  vCost: Extended;
  Costs: array of Extended;

begin
  Heap := TPairHeap.Create(@PairGreaterThan, 0);

  SetLength(Costs, VCount);
  for i := 0 to VCount - 1 do
    Costs[i] := Inf;

  Pair := TPair.Create(0, 0, -1);
  Heap.Insert(Pair);
  Costs[0] := 0;

  while Heap.Count <> 0 do
  begin
    Pair := Heap.Min;
    v := Pair.Node;
    vCost := Pair.Cost;
    Heap.DeleteMin;

    if Costs[v] < vCost then
    begin
      WriteLn(v, ' ', vCost);
      continue;
    end;


    for i := 0 to VCount - 1 do
    begin
      if vCost + Mat[v, i] < Costs[i] then
      begin
        Costs[i] := vCost + Mat[v, i];
        Pair := TPair.Create(i, Costs[i], v);
        Heap.Insert(Pair);
      end;
    end;

  end;

end;

{ TPair }

constructor TPair.Create(n: Integer; c: Extended; p: Integer);
begin
  inherited Create;

  Node := n;
  Cost := c;
  ParentNode := p;
end;

end.

