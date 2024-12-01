program Sample01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, MapReduce.GraphUnit, MapReduce.NodeUnit, MapReduce.ConfigUnit,
MapReduce.InputNodeUnit;

var
  g: MapReduce.GraphUnit.TGraph;
  n: TNode;

begin
  g := MapReduce.GraphUnit.TGraph.Create('Sample01');
  n := g.AddInput('/tmp', nil);
end.

