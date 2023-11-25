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
  id: Integer;

begin
  id := GetRunTimeParameterManager.ValueByName['--TaskID'].AsInteger;
  begin
    FMTDebugLn('id: %d', [id]);
    Step := TPipeline.TStepInfo.Create(0, 64, nil);
    Task := TTask.Create(id, Step);
    ExtractContentUnit.ExtractContent(Task);
    Step.Free;
    Task.Free;
    FMTDebugLn('id: %d', [id]);

  end;
end;

procedure TestExtractContentWithPipeline;
begin
  Pipeline := TPipeline.Create('Sample02',
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(1));

  Pipeline.AddNewStep(nil, 64);
  Pipeline.AddNewStep(@ExtractContent, 64);
  if Pipeline.Run then
    ALoggerUnit.FMTDebugLn('Success! [in %dms]', [DateTimeToTimeStamp(Now).Time -        Start])
  else
    ALoggerUnit.FatalLn('Failed!');

  Pipeline.Free;
end;

procedure TestFindStartIndices;
var
  Task: TTask;
  Step: TPipeline.TStepInfo;
  id: Integer;

begin
<<<<<<< HEAD
=======
  id := GetRunTimeParameterManager.ValueByName['--TaskID'].AsInteger;
  begin
    FMTDebugLn('id: %d', [id]);
    Step := TPipeline.TStepInfo.Create(0, 64, nil);
    Task := TTask.Create(id, Step);
    FindStartIndicesUnit.FindStartIndices(Task);
    Step.Free;
    Task.Free;
    FMTDebugLn('id: %d', [id]);

  end;
end;

begin
>>>>>>> bf15520 (...)
  if GetRunTimeParameterManager.ValueByName['--Mode'].AsAnsiString = 'TestExtractContent' then
  begin
    TestExtractContent;
    Exit;

  end
  else if GetRunTimeParameterManager.ValueByName['--Mode'].AsAnsiString = 'TestExtractContent' then
  begin
    TestExtractContentWithPipeline;
    Exit;

<<<<<<< HEAD
=======
  end
  else if GetRunTimeParameterManager.ValueByName['--Mode'].AsAnsiString = 'TestFindStartIndices' then
  begin
    TestFindStartIndices;
    Exit;

>>>>>>> bf15520 (...)
  end;

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

