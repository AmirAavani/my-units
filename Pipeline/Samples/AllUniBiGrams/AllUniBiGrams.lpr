program AllUniBiGrams;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, PipelineUnit, Pipeline.Utils, Pipeline.TypesUnit, ALoggerUnit,
  HeapUnit, sysutils, StepHandlersUnit, FindStartIndicesUnit, TypesUnit,
  FileHelperUnit, StreamUnit, ExtractContentUnit, SharedUnit, WikiParserUnit,
  WikiDocUnit, Laz2_DOM, WikiTypesUnits, WideStringUnit, SyncUnit,
  ProtoHelperUnit, ParamManagerUnit, ParamUnit;

var
  Pipeline: TPipeline;
  Start: Integer;

procedure TestExtractContent;
var
  Task: TTask;
  Step: TPipeline.TStepInfo;
  i, id: Integer;

begin
  id := Integer(ParamUnit.GetParams.Pipeline.TaskID.Value);
  for i := 1 to 64 do
  begin
    if (id <> -1) and (i <> id) then
      Continue;
    ALoggerUnit.GetLogger.FMTDebugLn('id: %d', [i]);
    Step := TPipeline.TStepInfo.Create(0, 64, nil);
    Task := TTask.Create(i, Step);
    ExtractContentUnit.ExtractContent(Task);
    Step.Free;
    Task.Free;
    ALoggerUnit.GetLogger.FMTDebugLn('id: %d', [i]);

  end;
end;

procedure TestExtractContentWithPipeline;
begin
  Pipeline := TPipeline.Create('Sample02',
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(1));

  Pipeline.AddNewStep(nil, 64);
  Pipeline.AddNewStep(@ExtractContent, 64);
  if Pipeline.Run then
    ALoggerUnit.GetLogger.FMTDebugLn('Success! [in %dms]', [DateTimeToTimeStamp(Now).Time -        Start])
  else
    ALoggerUnit.FmtFatalLnIFFalse(False, 'Failed!', []);

  Pipeline.Free;
end;

procedure TestFindStartIndices;
var
  Task: TTask;
  Step: TPipeline.TStepInfo;
  id: Integer;

begin
  id := Integer(ParamUnit.GetParams.Pipeline.TaskID.Value);
  begin
    ALoggerUnit.GetLogger.FMTDebugLn('id: %d', [id]);
    Step := TPipeline.TStepInfo.Create(0, 64, nil);
    Task := TTask.Create(id, Step);
    FindStartIndicesUnit.FindStartIndices(Task);
    Step.Free;
    Task.Free;
    ALoggerUnit.GetLogger.FMTDebugLn('id: %d', [id]);

  end;
end;

begin
  ALoggerUnit.InitLogger(ParamUnit.GetParams.Debug.Value);

  if ParamUnit.GetParams.Mode.Value = 'TestExtractContent' then
  begin
    TestExtractContent;
    Exit;

  end
  else if ParamUnit.GetParams.Mode.Value = 'TestExtractContentWithPipeline;' then
  begin
    TestExtractContentWithPipeline;
    Exit;

  end
  else if ParamUnit.GetParams.Mode.Value = 'TestFindStartIndices' then
  begin
    TestFindStartIndices;
    Exit;

  end;

  if not DirectoryExists(ParamUnit.GetParams.WorkingDir.Value) then
  begin
    FileHelperUnit.CreateDir(
      ParamUnit.GetParams.WorkingDir.Value,
      True
    );
  end;

  // Unigram/Bigram Count
  Pipeline := TPipeline.Create('Sample02',
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(16));

  AddStep1(Pipeline);
  AddStep2(Pipeline);

  Start := DateTimeToTimeStamp(Now).Time;

  if Pipeline.Run(ParamUnit.GetParams.Pipeline.StepID.Value)  then
    ALoggerUnit.GetLogger.FMTDebugLn('Success! [in %ums]', [DateTimeToTimeStamp(Now).Time - Start])
  else
    ALoggerUnit.FmtFatalLnIFFalse(False, 'Failed!', []);

  Pipeline.Free;

end.

