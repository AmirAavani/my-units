program AllUniBiGrams;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, PipelineUnit, Pipeline.Utils, Pipeline.TypesUnit, ALoggerUnit,
  HeapUnit, sysutils, StepHandlersUnit, FindStartIndicesUnit, TypesUnit,
  ParameterManagerUnit, FileHelperUnit, StreamUnit;

var
  Pipeline: TPipeline;
  Start: Integer;

begin
  if not DirectoryExists(GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString) then
  begin
    FileHelperUnit.CreateDir(
      GetRunTimeParameterManager.ValueByName['--WorkingDir'].AsAnsiString,
      True
    );
  end;

  // Unigram/Bigram Count
  Pipeline := TPipeline.Create('Sample02',
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(2));

  AddStep1(Pipeline);

  Start := DateTimeToTimeStamp(Now).Time;

  if Pipeline.Run then
    ALoggerUnit.FMTDebugLn('Success! [in %dms]', [DateTimeToTimeStamp(Now).Time - Start])
  else
    ALoggerUnit.FatalLn('Failed!');

  Pipeline.Free;

end.

