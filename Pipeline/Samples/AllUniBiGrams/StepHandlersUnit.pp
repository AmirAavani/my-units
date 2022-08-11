unit StepHandlersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  PipelineUnit, Pipeline.TypesUnit;

procedure AddStep1(Pipeline: TPipeline);

implementation
uses
  FindStartIndicesUnit, TypesUnit, ParameterManagerUnit;


procedure AddStep1(Pipeline: TPipeline);
begin
  Pipeline.AddNewStep(@FindStartIndices, 64);

end;

end.

