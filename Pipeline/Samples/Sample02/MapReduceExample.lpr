program MapReduceExample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, PipelineUnit, Pipeline.TypesUnit, WideStringUnit, ALoggerUnit,
  SyncUnit, ProtoHelperUnit, HeapUnit, sysutils,
  MapReduceUnit, StreamUnit, MapReduce.MapperUnit,
  MapReduce.KeyValueUnit, MapReduce.GraphUnit, MapReduce.ReaderUnit,
MapReduce.UtilsUnits, StepHandlerUnit;

function f(constref KV: MapReduce.KeyValueUnit.TKeyValue; Sink: TSink): boolean;
begin
  Result := True;
  if Length(kv.Value) mod 2 = 0 then
    Exit;
  Sink.Put(KV);
end;

function g(constref KV: MapReduce.KeyValueUnit.TKeyValue; Sink: TSink): boolean;
begin

end;



function BuildGraph: TGraph;
var
  Input, o1: MapReduce.GraphUnit.TNode;

begin
  Result := TGraph.Create('g1');
  Input := Result.AddInput('/tmp/p11.pdf', TTextLineReader.Create('/tmp/p11.pdf'));

  o1 := Input.Reshard(16).Map(@f).Reshard(32).Map(@g).Reshard(16);
  o1.
    SetDestination('/tmp/o1@16');

  Result.MustCompile;
end;



var
  Pipeline: TPipeline;
  Start: Integer;
  Graph: TGraph;

begin
  Graph := BuildGraph;
  Pipeline := Graph.ExportAsPipeline(
    TPipelineConfig.DefaultConfig.SetNumberOfThreads(16)
  );
  Graph.Free;


  Start := DateTimeToTimeStamp(Now).Time;

  if Pipeline.Run then
    ALoggerUnit.GetLogger.FMTDebugLn('Success! [in %dms]', [DateTimeToTimeStamp(Now).Time - Start])
  else
    ALoggerUnit.FmtFatalLnIFFalse(False, 'Failed!', []);

  Pipeline.Free;

end.

