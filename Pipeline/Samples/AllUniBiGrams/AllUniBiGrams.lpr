program AllUniBiGrams;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, PipelineUnit, Pipeline.Utils, Pipeline.TypesUnit, ALoggerUnit,
  HeapUnit, sysutils, StepHandlersUnit, FindStartIndicesUnit, TypesUnit,
  ParameterManagerUnit, FileHelperUnit, StreamUnit, ExtractContentUnit,
  SharedUnit, WikiParserUnit, WikiDocUnit, Laz2_DOM, WikiTypesUnits,
  WideStringUnit, SyncUnit, ProtoHelperUnit;

var
  Pipeline: TPipeline;
  Start: Integer;

procedure TestExtractContent;
var
  Task: TTask;
  Step: TPipeline.TStepInfo;

begin
  Step := TPipeline.TStepInfo.Create(2, 64, nil);
  Task := TTask.Create(1, Step);
  ExtractContentUnit.ExtractContent(Task);
  Step.Free;
  Task.Free;
end;

begin
  TestExtractContent;
  Exit;

  if not DirectoryExists(GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString) then
  begin
    FileHelperUnit.CreateDir(
      GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
      True
    );
  end;

  // Unigram/Bigram Count
  Pipeline := TPipeline.Create('Sample02',
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(16));

  AddStep1(Pipeline);
  AddStep2(Pipeline);

  Start := DateTimeToTimeStamp(Now).Time;

  if Pipeline.Run then
    ALoggerUnit.FMTDebugLn('Success! [in %dms]', [DateTimeToTimeStamp(Now).Time - Start])
  else
    ALoggerUnit.FatalLn('Failed!');

  Pipeline.Free;

end.

