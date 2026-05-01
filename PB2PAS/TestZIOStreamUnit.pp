program TestZIOStreamUnit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ZIOStreamUnit, ProtoHelperUnit;

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
  if FValue <> 0 then
    Stream.WriteInt32(1, FValue);
  if FText <> '' then
    Stream.WriteString(2, FText);
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
      1: FValue := Stream.ReadInt32;
      2: FText := Stream.ReadString;
    else
      Stream.SkipField(WireType);
    end;
  end;
  Result := Stream.Position = StartPos + Len;
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

procedure TestZIOReadWrite;
var
  Writer: specialize TZioWriter<TTestMessage>;
  Reader: specialize TZioReader<TTestMessage>;
  Pattern: TPattern;
  Msg, ReadMsg: TTestMessage;
  TempDir: AnsiString;
  i: Integer;
begin
  WriteLn('=== TestZIOReadWrite ===');
  
  TempDir := '/tmp/zio_test_' + IntToStr(Random(1000000));
  ForceDirectories(TempDir);
  
  // Write messages
  Pattern := TPattern.Create(TempDir, 4);
  Writer := specialize TZioWriter<TTestMessage>.Create(Pattern);
  
  for i := 0 to 9 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'Message ' + IntToStr(i);
    Writer.WriteMessageToShard(Msg, i mod 4);  // Distribute across shards
    Msg.Free;
  end;
  
  Writer.Free;
  
  // Read messages back
  Reader := specialize TZioReader<TTestMessage>.Create(Pattern);
  Pattern.Free;
  
  ReadMsg := TTestMessage.Create;
  i := 0;
  while Reader.ReadMessage(ReadMsg) do
  begin
    AssertTrue(ReadMsg.Value >= 0, 'Read value should be valid');
    AssertTrue(ReadMsg.Text <> '', 'Read text should not be empty');
    Inc(i);
    ReadMsg.Clear;
  end;
  
  AssertEquals(10, i, 'Should read 10 messages');
  
  ReadMsg.Free;
  Reader.Free;
  
  // Cleanup
  DeleteDirectory(TempDir, False);
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
  TestZIOReadWrite;
  
  WriteLn('===========================');
  WriteLn(Format('Tests Passed: %d', [TestsPassed]));
  WriteLn(Format('Tests Failed: %d', [TestsFailed]));
  WriteLn('');
  
  if TestsFailed > 0 then
    Halt(1)
  else
    WriteLn('All tests passed!');
end.
