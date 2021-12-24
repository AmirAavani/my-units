{$mode objfpc}
unit FlowUnit;
interface
uses
  fgl;
type
  TGraph = array of array of Integer;
  TIntList = specialize TFPGList<Integer>;

  function FordFulkerson(Graph: TGraph; s, t: Integer): Integer;

implementation
uses
  Math;

{ Returns true if there is a path from source 's' to sink 't' in
  residual graph. Also fills parent[] to store the path }
function BFS(var Graph: TGraph; s, t: Integer; Parents: TIntList): Boolean;
var
  Visited: array of Boolean;
  Q: TIntList;
  FoQ: Integer;
  vCount: Integer;
  u, v: Integer;
  i: Integer;

begin
  vCount := Length(Graph);
  SetLength(Visited, vCount);
  FillChar(Visited[0], vCount * SizeOf(Visited[0]), 0);
 
  Parents.Count := vCount;
  for i := 0 to vCount - 1 do
    Parents[i] := -1;
 
  // Create a queue, enqueue source vertex and mark source vertex as visited
   Q := TIntList.Create;
   Q.Add(s);
   FoQ := 0;
   Visited[s] := true;
 
    while FoQ < Q.Count do
    begin
       u := Q[FoQ];
       Inc(FoQ);
 
       for v := 0 to vCount - 1 do
       begin 
         if not Visited[v] and (Graph[u][v] > 0) then
         begin 
           Q.Add(v);
           Parents[v] := u;
           Visited[v] := true;
         end; 
       end; 
    end;
 
    Result := Visited[t];
end;
 
// Returns the maximum flow from s to t in the given graph
function FordFulkerson(Graph: TGraph; s, t: Integer): Integer;
var
  u, v: Integer;
  vCount: Integer;
  rGraph: TGraph;
  Parents: TIntList;
  MaxFlow, PathFlow: Integer;

begin
   vCount := Length(Graph);
   SetLength(rGraph, vCount);
   for u := 0 to vCount - 1 do
   begin
     SetLength(rGraph[u], vCount);
     for v := 0 to vCount - 1 do
       rGraph[u][v] := Graph[u][v];
   end;
   Parents := TIntList.Create;
 
    MaxFlow := 0;
 
    // Augment the flow while tere is path from source to sink
    while (BFS(rGraph, s, t, Parents)) do
    begin
       // Find minimum residual capacity of the edges along the
       // path filled by BFS. Or we can say find the maximum flow
       // through the path found.
       PathFlow := MAXINT;
 
       v := t;
       while v <> s do 
       begin
         u := Parents[v];
         PathFlow := Min(PathFlow, RGraph[u][v]);
         v := Parents[v];
       end;
 
       // update residual capacities of the edges and reverse edges
       // along the path
       v := t;
       while v <> s do 
       begin
         rGraph[u][v] := rGraph[u][v] - PathFlow;
         rGraph[v][u] := rGraph[v][u] + PathFlow;
         v := Parents[v];
       end;
 
       // Add path flow to overall flow
       Inc(MaxFlow, PathFlow);
    end;
 
    Result := MaxFlow;
end;

var
  G: TGraph; 
  i: Integer;
begin
  SetLength(G, 6);
  for i := 0 to High(G) do
  begin
    SetLength(G[i], Length(G));
    FillChar(G[i][0], Length(G) * SizeOf(G[i][0]), 0);
  end;
  { 0, 16, 13, 0, 0, 0,
    0, 0, 10, 12, 0, 0,
    0, 4, 0, 0, 14, 0,
    0, 0, 9, 0, 0, 20,
    0, 0, 0, 7, 0, 4,
    0, 0, 0, 0, 0, 0
  }
  G[0][1] := 16;
  G[0][2] := 13;
  G[1][2] := 10;
  G[1][3] := 12;
  G[2][1] := 4;
  G[2][4] := 14;
  G[3][2] := 9;
  G[3][5] := 20;
  G[4][3] := 7;
  G[4][5] := 4;
  
  WriteLn(FordFulkerson(G, 0, 5));
 
end.
