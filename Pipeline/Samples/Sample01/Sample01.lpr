program Sample01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, PipelineUnit, Pipeline.TypesUnit, WideStringUnit, ALoggerUnit,
  SyncUnit, ProtoHelperUnit, HeapUnit, sysutils, StepHandlersUnit, MapReduceUnit
  { you can add units after this };

var
  Pipeline: TPipeline;
  Start: Integer;

begin
  Pipeline := TPipeline.Create('Sample01',
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(16));

  Pipeline.AddNewStep(@Step1Hanlder, 10);
  Pipeline.AddNewStep(@Steps2And3Hanlder, 20);
  Pipeline.AddNewStep(@Steps2And3Hanlder, 100);

  Start := DateTimeToTimeStamp(Now).Time;

  if Pipeline.Run then
    ALoggerUnit.GetLogger.FMTDebugLn('Success! [in %dms]', [DateTimeToTimeStamp(Now).Time - Start])
  else
    ALoggerUnit.FmtFatalLnIFFalse(False, 'Failed!', []);

  Pipeline.Free;

end.

