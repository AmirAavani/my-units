program BenchmarkZIOBuffering;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ZIOStreamUnit, ProtoHelperUnit, ProtoStreamUnit, DateUtils;

type
  { Simple test message }
  TBenchMessage = class(TBaseMessage)
  private
    FID: Int32;
    FName: AnsiString;
    FValue: Double;
  public
    property ID: Int32 read FID write FID;
    property Name: AnsiString read FName write FName;
    property Value: Double read FValue write FValue;
    
    procedure Clear; override;
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
  end;

{ TBenchMessage }

procedure TBenchMessage.Clear;
begin
  FID := 0;
  FName := '';
  FValue := 0.0;
end;

procedure TBenchMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveInt32(Stream, FID, 1);
  SaveString(Stream, FName, 2);
  SaveDouble(Stream, FValue, 3);
end;

function TBenchMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;
begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);
    case FieldNumber of
      1: FID := LoadInt32(Stream);
      2: FName := LoadString(Stream);
      3: FValue := LoadDouble(Stream);
    else
      SkipField(Stream, WireType);
    end;
  end;
  Result := StartPos + Len = Stream.Position;
end;

{ Benchmark procedures }

procedure BenchmarkWrite(const TestName: AnsiString; NumMessages: Integer);
var
  Pattern: TPattern;
  Writer: specialize TZioWriter<TBenchMessage>;
  Msg: TBenchMessage;
  i: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;
  TempDir: AnsiString;
  MsgPerSec: Double;
begin
  TempDir := GetTempDir + 'ziobench_' + IntToStr(Random(100000));
  WriteLn('TEMPDIR: ', TempDir);
  
  Pattern := TPattern.Create(TempDir, 4);
  try
    Writer := specialize TZioWriter<TBenchMessage>.Create(Pattern);
    try
      StartTime := Now;
      
      for i := 1 to NumMessages do
      begin
        Msg := TBenchMessage.Create;
        try
          Msg.ID := i;
          Msg.Name := Format('Message number %d', [i]);
          Msg.Value := i * 3.14159;
          Writer.WriteMessage(Msg);
        finally
          Msg.Free;
        end;
      end;
      
      EndTime := Now;
      ElapsedMs := MilliSecondsBetween(EndTime, StartTime);
      MsgPerSec := (NumMessages / ElapsedMs) * 1000;
      
      WriteLn(Format('%-40s: %6d ms (%8.0f msg/sec)', 
                    [TestName, ElapsedMs, MsgPerSec]));
      
    finally
      Writer.Free;
    end;
  finally
    Pattern.Free;
    // Cleanup temp directory
    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);
  end;
end;

procedure BenchmarkRead(const TestName: AnsiString; NumMessages: Integer);
var
  Pattern: TPattern;
  Writer: specialize TZioWriter<TBenchMessage>;
  Reader: specialize TZioReader<TBenchMessage>;
  Msg: TBenchMessage;
  i: Integer;
  StartTime, EndTime: TDateTime;
  ElapsedMs: Int64;
  TempDir: AnsiString;
  MsgPerSec: Double;
  Count: Integer;
  Paths: TAnsiStringArray;
begin
  TempDir := GetTempDir + 'ziobench_' + IntToStr(Random(100000));
  Pattern := TPattern.Create(TempDir, 4);
  try
    // First write the data
    Writer := specialize TZioWriter<TBenchMessage>.Create(Pattern);
    try
      for i := 1 to NumMessages do
      begin
        Msg := TBenchMessage.Create;
        try
          Msg.ID := i;
          Msg.Name := Format('Message number %d', [i]);
          Msg.Value := i * 3.14159;
          Writer.WriteMessage(Msg);
        finally
          Msg.Free;
        end;
      end;
    finally
      Writer.Free;
    end;
    
    // Get paths AFTER writer is freed (files are closed)
    Paths := Pattern.GetAllPaths;
    
    // Now benchmark reading
    Reader := specialize TZioReader<TBenchMessage>.Create(Paths);
    try
      Msg := TBenchMessage.Create;
      try
        Count := 0;
        StartTime := Now;
        
        while Reader.ReadMessage(Msg) do
        begin
          Inc(Count);
          Msg.Clear;
        end;
        
        EndTime := Now;
        ElapsedMs := MilliSecondsBetween(EndTime, StartTime);
        MsgPerSec := (Count / ElapsedMs) * 1000;
        
        WriteLn(Format('%-40s: %6d ms (%8.0f msg/sec)', 
                      [TestName, ElapsedMs, MsgPerSec]));
        
      finally
        Msg.Free;
      end;
    finally
      Reader.Free;
    end;
    
  finally
    Pattern.Free;
    // Cleanup temp directory
    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);
  end;
end;

{ Main }

begin
  Randomize;
  
  WriteLn('===========================================');
  WriteLn('  ZIOStream Buffering Benchmark');
  WriteLn('  (WITH buffering - 64KB write, 128KB read)');
  WriteLn('===========================================');
  WriteLn;
  
  try
    WriteLn('WRITE Performance:');
    BenchmarkWrite('  1,000 messages', 1000);
    BenchmarkWrite('  10,000 messages', 10000);
    BenchmarkWrite('  50,000 messages', 50000);
    WriteLn('DOne');
    WriteLn;
    WriteLn('READ Performance:');
    BenchmarkRead('  1,000 messages', 1000);
    BenchmarkRead('  10,000 messages', 10000);
    BenchmarkRead('  50,000 messages', 50000);
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
  WriteLn('===========================================');
  WriteLn('  Benchmark completed!');
  WriteLn('===========================================');
end.
