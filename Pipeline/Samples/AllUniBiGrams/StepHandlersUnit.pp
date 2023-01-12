unit StepHandlersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  PipelineUnit;

procedure AddStep1(Pipeline: TPipeline);
procedure AddStep2(Pipeline: TPipeline);

implementation
uses
  FindStartIndicesUnit, ParameterManagerUnit, ExtractContentUnit;


procedure AddStep1(Pipeline: TPipeline);
begin
  Pipeline.AddNewStep(@FindStartIndices, 64);

end;

procedure AddStep2(Pipeline: TPipeline);
begin
  Pipeline.AddNewStep(@ExtractContent, 64);

end;

end.

