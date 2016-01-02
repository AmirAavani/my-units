unit SpanningTreeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAdjMat = array of array of Extended;
  TTreeEdge = record
    Node: Integer;
    Parent: Integer;
    Weight: Extended;
  end;
  TTree = array of TTreeEdge;

function MinWeightSpanningTree(const Mat: TAdjMat; VCount: Integer; var SpanningTree: TTree): Boolean;

implementation
uses fgl, Math;

function MinWeightSpanningTree(const Mat: TAdjMat; VCount: Integer; var SpanningTree: TTree): Boolean;
const
  Inf = 1e100;

var
  i, j: Integer;
  MinIndex: Integer;
  MinCost: Extended;
  tCount: Integer;
  vCost: Extended;
  Selected: array of Boolean;
  Costs: array of Extended;
  Parents: array of Integer;

begin
  Result := True;
  SetLength(Costs, VCount);
  SetLength(Parents, VCount);
  SetLength(Selected, VCount);

  SetLength(SpanningTree, VCount - 1);
  FillChar(SpanningTree[0], SizeOf(SpanningTree), 0);
  FillChar(Selected[0], SizeOf(Selected), 0);
  FillChar(Parents[0], SizeOf(Parents), 255);
  for i := 0 to VCount - 1 do
    Costs[i] := Inf;

  Costs[0] := 0;
  Parents[0] := 0;

  for i := 1 to VCount - 1 do
  begin
    MinIndex := 0;
    MinCost := Inf;

    for j := 0 to VCount - 1 do
      if (not Selected[j]) and (Costs[j] < MinCost) then
      begin
        MinIndex := j;
        MinCost := Costs[j];
      end;

    if MinCost = Inf then
    begin
      Result := False;
      Break;
    end;

    for j := 0 to VCount - 1 do
      if (Mat[MinIndex][j] < Costs[j]) and (not Selected[j]) then
      begin
        Costs[j] := Mat[MinIndex][j] ;
        Parents[j] := MinIndex;
      end;
    Selected[MinIndex] := True;
  end;

  SetLength(SpanningTree, VCount - 1);
  for j := 1 to VCount - 1 do
  begin
    SpanningTree[j - 1].Node := j;
    SpanningTree[j - 1].Parent := Parents[j];
    SpanningTree[j - 1].Weight := Mat[Parents[j]][j];
  end;

  SetLength(Costs, 0);
  SetLength(Parents, 0);
  SetLength(Selected, 0);

end;

end.

