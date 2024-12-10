program Sample01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, MapReduce.GraphUnit, MapReduce.NodeUnit, MapReduce.ConfigUnit,
  MapReduce.InputMappersUnit, MapReduce.TypesUnit, SimpleTypesUnit, MapperUnits,
  MapReduce.FuncMapperUnit, MapReduce.MappersUnit, MapReduce.OutputMappersUnit;

var
  g: MapReduce.GraphUnit.TGraph;
  n: TNode;
  Config: TRunConfig;

begin
  g := MapReduce.GraphUnit.TGraph.Create('Sample01');
  n := g.StartingNode.Map(TTextInputMapper.Create('This is Text1 .'));
  //n2 := g.StartingNode.Map(TTextInputMapper.Create('This is Text2.'));
  //n3 := g.StartingNode.Map(TTextInputMapper.Create('This is Text3.'));
  n := n.Map(TSplitByDelimiterMapper.Create(' '));
  n := n.Map(TOutputWriteLnMapper.Create);
  //n2.Map(TSplitByMapper.Create(' '));
  //n3.Map(TSplitByMapper.Create(' '));
  Config.Init.SetNumThreads(8);

  g.MustCompile;
  g.Describe;
  g.Run(Config);

end.

