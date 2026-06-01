program TestZIOStreamUnit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ZIOStreamUnit, ProtoHelperUnit, ProtoStreamUnit;

type
  { Simple test message }
  TTestMessage = class(TBaseMessage)
  private
    FValue: Int32;
    FText: AnsiString;
  public
    property Value: Int32 read FValue write FValue;
    property Text: AnsiString read FText write FText;
    
    procedure Clear; override;
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
  end;

procedure TTestMessage.Clear;
begin
  FValue := 0;
  FText := '';
end;

procedure TTestMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveInt32(Stream, FValue, 1);
  SaveString(Stream, FText, 2);
end;

function TTestMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;
begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);
    case FieldNumber of
      1: FValue := LoadInt32(Stream);
      2: FText := LoadString(Stream);
    else
      SkipField(Stream, WireType);
    end;
  end;
  Result := StartPos + Len = Stream.Position;
end;

{ Test Suite }

var
  TestsPassed, TestsFailed: Integer;

procedure AssertTrue(Condition: Boolean; const TestName: AnsiString);
begin
  if Condition then
  begin
    WriteLn('[PASS] ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', TestName);
    Inc(TestsFailed);
  end;
end;

procedure AssertEquals(Expected, Actual: Integer; const TestName: AnsiString);
begin
  AssertTrue(Expected = Actual, TestName + Format(' (expected %d, got %d)', [Expected, Actual]));
end;

procedure AssertEquals(const Expected, Actual: AnsiString; const TestName: AnsiString);
begin
  AssertTrue(Expected = Actual, TestName + Format(' (expected "%s", got "%s")', [Expected, Actual]));
end;

procedure TestTPatternBasic;
var
  Pattern: TPattern;
  Path: AnsiString;
begin
  WriteLn('=== TestTPatternBasic ===');
  
  // Test constructor with path and num shards
  Pattern := TPattern.Create('/tmp/test', 16);
  
  AssertEquals('/tmp/test', Pattern.BasePath, 'BasePath should match');
  AssertEquals(16, Pattern.NumShards, 'NumShards should be 16');
  
  // Test GetShardPath
  Path := Pattern.GetShardPath(0);
  AssertEquals('/tmp/test/shards-0000-0016.zio', Path, 'First shard path');
  
  Path := Pattern.GetShardPath(15);
  AssertEquals('/tmp/test/shards-0015-0016.zio', Path, 'Last shard path');
  
  Pattern.Free;
  WriteLn('');
end;

procedure TestTPatternModulo;
var
  Pattern, FilteredPattern: TPattern;
  Path: AnsiString;
begin
  WriteLn('=== TestTPatternModulo ===');
  
  // Create pattern with 16 shards
  Pattern := TPattern.Create('/tmp/test', 16);
  
  // Filter to get shards where (index mod 4 = 0) -> shards 0, 4, 8, 12
  FilteredPattern := Pattern.WithModulo(0, 4);
  
  AssertEquals(4, FilteredPattern.NumShards, 'Filtered should have 4 shards');
  
  // Logical index 0 -> actual shard 0
  Path := FilteredPattern.GetShardPath(0);
  AssertEquals('/tmp/test/shards-0000-0016.zio', Path, 'Filtered shard 0');
  
  // Logical index 1 -> actual shard 4
  Path := FilteredPattern.GetShardPath(1);
  AssertEquals('/tmp/test/shards-0004-0016.zio', Path, 'Filtered shard 1');
  
  // Logical index 2 -> actual shard 8
  Path := FilteredPattern.GetShardPath(2);
  AssertEquals('/tmp/test/shards-0008-0016.zio', Path, 'Filtered shard 2');
  
  // Logical index 3 -> actual shard 12
  Path := FilteredPattern.GetShardPath(3);
  AssertEquals('/tmp/test/shards-0012-0016.zio', Path, 'Filtered shard 3');
  
  FilteredPattern.Free;
  Pattern.Free;
  WriteLn('');
end;

procedure TestTPatternModuloDifferentRemainder;
var
  Pattern, FilteredPattern: TPattern;
  Path: AnsiString;
begin
  WriteLn('=== TestTPatternModuloDifferentRemainder ===');
  
  // Create pattern with 16 shards
  Pattern := TPattern.Create('/tmp/test', 16);
  
  // Filter to get shards where (index mod 4 = 1) -> shards 1, 5, 9, 13
  FilteredPattern := Pattern.WithModulo(1, 4);
  
  AssertEquals(4, FilteredPattern.NumShards, 'Filtered should have 4 shards');
  
  // Logical index 0 -> actual shard 1
  Path := FilteredPattern.GetShardPath(0);
  AssertEquals('/tmp/test/shards-0001-0016.zio', Path, 'Filtered shard 0 (rem=1)');
  
  // Logical index 1 -> actual shard 5
  Path := FilteredPattern.GetShardPath(1);
  AssertEquals('/tmp/test/shards-0005-0016.zio', Path, 'Filtered shard 1 (rem=1)');
  
  FilteredPattern.Free;
  Pattern.Free;
  WriteLn('');
end;

procedure TestTPatternModuloUneven;
var
  Pattern, FilteredPattern: TPattern;
begin
  WriteLn('=== TestTPatternModuloUneven ===');
  
  // Create pattern with 10 shards, filter by mod 3
  Pattern := TPattern.Create('/tmp/test', 10);
  
  // Filter to get shards where (index mod 3 = 0) -> shards 0, 3, 6, 9 (4 shards)
  FilteredPattern := Pattern.WithModulo(0, 3);
  AssertEquals(4, FilteredPattern.NumShards, 'Filtered should have 4 shards (0,3,6,9)');
  
  FilteredPattern.Free;
  
  // Filter to get shards where (index mod 3 = 1) -> shards 1, 4, 7 (3 shards)
  FilteredPattern := Pattern.WithModulo(1, 3);
  AssertEquals(3, FilteredPattern.NumShards, 'Filtered should have 3 shards (1,4,7)');
  
  FilteredPattern.Free;
  
  // Filter to get shards where (index mod 3 = 2) -> shards 2, 5, 8 (3 shards)
  FilteredPattern := Pattern.WithModulo(2, 3);
  AssertEquals(3, FilteredPattern.NumShards, 'Filtered should have 3 shards (2,5,8)');
  
  FilteredPattern.Free;
  Pattern.Free;
  WriteLn('');
end;

procedure TestMultiPartWriters;
var
  Writer: specialize TZioWriter<TTestMessage>;
  PartWriter1, PartWriter2, PartWriter3: specialize TZioWriter<TTestMessage>.TZioPartWriter;
  Pattern: TPattern;
  Msg: TTestMessage;
  TempDir: AnsiString;
  i: Integer;
  HasPart1, HasPart2, HasPart3: Boolean;
begin
  WriteLn('=== TestMultiPartWriters ===');
  
  TempDir := '/tmp/zio_multipart_test_' + IntToStr(Random(1000000));
  ForceDirectories(TempDir);
  
  // Create writer with 4 shards
  Pattern := TPattern.Create(TempDir, 4);
  Writer := specialize TZioWriter<TTestMessage>.Create(Pattern);
  
  // Create three part writers (round-robin across shards)
  PartWriter1 := Writer.NewPartWriter;  // shard 0
  PartWriter2 := Writer.NewPartWriter;  // shard 1
  PartWriter3 := Writer.NewPartWriter;  // shard 2
  
  // Verify part paths are not empty
  HasPart1 := PartWriter1.PartPath <> '';
  HasPart2 := PartWriter2.PartPath <> '';
  HasPart3 := PartWriter3.PartPath <> '';
  
  AssertTrue(HasPart1, 'Part 1 path should be generated');
  AssertTrue(HasPart2, 'Part 2 path should be generated');
  AssertTrue(HasPart3, 'Part 3 path should be generated');
  
  // Write to first part
  for i := 0 to 2 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'Part1-Message-' + IntToStr(i);
    PartWriter1.WriteMessage(Msg);
    Msg.Free;
  end;
  
  // Write to second part
  for i := 10 to 12 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'Part2-Message-' + IntToStr(i);
    PartWriter2.WriteMessage(Msg);
    Msg.Free;
  end;
  
  // Write to third part
  for i := 20 to 22 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'Part3-Message-' + IntToStr(i);
    PartWriter3.WriteMessage(Msg);
    Msg.Free;
  end;
  
  // Verify files exist
  AssertTrue(FileExists(PartWriter1.PartPath), 'Part 1 file should exist');
  AssertTrue(FileExists(PartWriter2.PartPath), 'Part 2 file should exist');
  AssertTrue(FileExists(PartWriter3.PartPath), 'Part 3 file should exist');
  
  // Cleanup
  PartWriter1.Free;
  PartWriter2.Free;
  PartWriter3.Free;
  Writer.Free;
  Pattern.Free;
  
  // Cleanup directory
  WriteLn('  Note: Cleanup directory manually if needed: ', TempDir);
  WriteLn('');
end;

procedure TestMultiplePartsInSameShard;
var
  Writer: specialize TZioWriter<TTestMessage>;
  PartWriter1, PartWriter2: specialize TZioWriter<TTestMessage>.TZioPartWriter;
  TempPW: specialize TZioWriter<TTestMessage>.TZioPartWriter;
  Pattern: TPattern;
  Msg: TTestMessage;
  TempDir: AnsiString;
  i: Integer;
  Path1, Path2: AnsiString;
begin
  WriteLn('=== TestMultiplePartsInSameShard ===');
  
  TempDir := '/tmp/zio_sameshard_test_' + IntToStr(Random(1000000));
  ForceDirectories(TempDir);
  
  // Create writer with 2 shards
  Pattern := TPattern.Create(TempDir, 2);
  Writer := specialize TZioWriter<TTestMessage>.Create(Pattern);
  
  // Create first part writer (shard 0)
  PartWriter1 := Writer.NewPartWriter;
  Path1 := PartWriter1.PartPath;
  
  // Skip shard 1
  TempPW := Writer.NewPartWriter;
  TempPW.Free;
  
  // Create second part writer (shard 0 again)
  PartWriter2 := Writer.NewPartWriter;
  Path2 := PartWriter2.PartPath;
  
  // Verify both parts are in shard 0
  AssertTrue(Pos('shard-0000-of-0002', Path1) > 0, 'Part 1 should be in shard 0');
  AssertTrue(Pos('shard-0000-of-0002', Path2) > 0, 'Part 2 should be in shard 0');
  
  // Verify different filenames (different sequence numbers)
  AssertTrue(Path1 <> Path2, 'Part files should have different names');
  
  // Write to both parts
  for i := 0 to 2 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'FirstPart-' + IntToStr(i);
    PartWriter1.WriteMessage(Msg);
    Msg.Free;
  end;
  
  for i := 100 to 102 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'SecondPart-' + IntToStr(i);
    PartWriter2.WriteMessage(Msg);
    Msg.Free;
  end;
  
  // Verify both files exist
  AssertTrue(FileExists(Path1), 'Part 1 file should exist');
  AssertTrue(FileExists(Path2), 'Part 2 file should exist');
  
  // Cleanup
  PartWriter1.Free;
  PartWriter2.Free;
  Writer.Free;
  Pattern.Free;
  
  WriteLn('  Note: Cleanup directory manually if needed: ', TempDir);
  WriteLn('');
end;

procedure TestPartWriterRoundRobin;
var
  Writer: specialize TZioWriter<TTestMessage>;
  PartWriter1, PartWriter2, PartWriter3, PartWriter4, PartWriter5: specialize TZioWriter<TTestMessage>.TZioPartWriter;
  Pattern: TPattern;
  TempDir: AnsiString;
  Path1, Path2, Path3, Path4, Path5: AnsiString;
begin
  WriteLn('=== TestPartWriterRoundRobin ===');
  
  TempDir := '/tmp/zio_roundrobin_test_' + IntToStr(Random(1000000));
  ForceDirectories(TempDir);
  
  // Create writer with 3 shards
  Pattern := TPattern.Create(TempDir, 3);
  Writer := specialize TZioWriter<TTestMessage>.Create(Pattern);
  
  // Create 5 part writers, should cycle: shard 0, 1, 2, 0, 1
  PartWriter1 := Writer.NewPartWriter;
  PartWriter2 := Writer.NewPartWriter;
  PartWriter3 := Writer.NewPartWriter;
  PartWriter4 := Writer.NewPartWriter;
  PartWriter5 := Writer.NewPartWriter;
  
  Path1 := PartWriter1.PartPath;
  Path2 := PartWriter2.PartPath;
  Path3 := PartWriter3.PartPath;
  Path4 := PartWriter4.PartPath;
  Path5 := PartWriter5.PartPath;
  
  // Verify round-robin distribution
  AssertTrue(Pos('shard-0000-of-0003', Path1) > 0, 'Part 1 should be in shard 0');
  AssertTrue(Pos('shard-0001-of-0003', Path2) > 0, 'Part 2 should be in shard 1');
  AssertTrue(Pos('shard-0002-of-0003', Path3) > 0, 'Part 3 should be in shard 2');
  AssertTrue(Pos('shard-0000-of-0003', Path4) > 0, 'Part 4 should be in shard 0 (wrap around)');
  AssertTrue(Pos('shard-0001-of-0003', Path5) > 0, 'Part 5 should be in shard 1 (wrap around)');
  
  // Cleanup
  PartWriter1.Free;
  PartWriter2.Free;
  PartWriter3.Free;
  PartWriter4.Free;
  PartWriter5.Free;
  Writer.Free;
  Pattern.Free;
  
  WriteLn('  Note: Cleanup directory manually if needed: ', TempDir);
  WriteLn('');
end;

begin
  TestsPassed := 0;
  TestsFailed := 0;
  
  Randomize;
  
  WriteLn('Running ZIOStreamUnit Tests');
  WriteLn('===========================');
  WriteLn('');
  
  TestTPatternBasic;
  TestTPatternModulo;
  TestTPatternModuloDifferentRemainder;
  TestTPatternModuloUneven;
  TestMultiPartWriters;
  TestMultiplePartsInSameShard;
  TestPartWriterRoundRobin;
  
  WriteLn('===========================');
  WriteLn(Format('Tests Passed: %d', [TestsPassed]));
  WriteLn(Format('Tests Failed: %d', [TestsFailed]));
  WriteLn('');
  
  if TestsFailed > 0 then
    Halt(1)
  else
    WriteLn('All tests passed!');
end.
