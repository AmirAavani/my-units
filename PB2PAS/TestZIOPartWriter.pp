program TestZIOPartWriter;

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

{ Test TZioPartWriter }

procedure TestMultiPartWriters;
var
  Writer: specialize TZioWriter<TTestMessage>;
  PartWriter1, PartWriter2, PartWriter3: specialize TZioWriter<TTestMessage>.TZioPartWriter;
  Pattern: TPattern;
  Msg: TTestMessage;
  TempDir: AnsiString;
  i: Integer;
begin
  WriteLn('=== TestMultiPartWriters ===');
  
  TempDir := '/tmp/zio_multipart_test_' + IntToStr(Random(1000000));
  WriteLn('Creating test directory: ', TempDir);
  ForceDirectories(TempDir);
  
  // Create writer with 4 shards
  Pattern := TPattern.Create(TempDir, 4);
  Writer := specialize TZioWriter<TTestMessage>.Create(Pattern);
  
  WriteLn('Creating 3 part writers (round-robin across shards)...');
  
  // Create first part writer (shard 0)
  PartWriter1 := Writer.NewPartWriter;
  WriteLn('  Part 1: ', PartWriter1.PartPath);
  
  // Create second part writer (shard 1)
  PartWriter2 := Writer.NewPartWriter;
  WriteLn('  Part 2: ', PartWriter2.PartPath);
  
  // Create third part writer (shard 2)
  PartWriter3 := Writer.NewPartWriter;
  WriteLn('  Part 3: ', PartWriter3.PartPath);
  
  WriteLn('Writing messages to part writers...');
  
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
  
  WriteLn('Cleaning up...');
  
  // Caller frees part writers
  PartWriter1.Free;
  PartWriter2.Free;
  PartWriter3.Free;
  
  Writer.Free;
  Pattern.Free;
  
  WriteLn('Test completed successfully!');
  WriteLn('Check directory: ', TempDir);
  WriteLn('');
end;

procedure TestMultiplePartsInSameShard;
var
  Writer: specialize TZioWriter<TTestMessage>;
  PartWriter1, PartWriter2: specialize TZioWriter<TTestMessage>.TZioPartWriter;
  Pattern: TPattern;
  Msg: TTestMessage;
  TempDir: AnsiString;
  i: Integer;
begin
  WriteLn('=== TestMultiplePartsInSameShard ===');
  
  TempDir := '/tmp/zio_samepart_test_' + IntToStr(Random(1000000));
  WriteLn('Creating test directory: ', TempDir);
  ForceDirectories(TempDir);
  
  // Create writer with 2 shards
  Pattern := TPattern.Create(TempDir, 2);
  Writer := specialize TZioWriter<TTestMessage>.Create(Pattern);
  
  WriteLn('Creating 2 part writers for shard 0...');
  
  // Create first part writer (shard 0)
  PartWriter1 := Writer.NewPartWriter;
  WriteLn('  Part 1: ', PartWriter1.PartPath);
  
  // Create another part writer (shard 1, but we'll create another for shard 0)
  // Skip shard 1 by calling NewPartWriter
  Writer.NewPartWriter.Free; // shard 1
  
  // Now create second part writer for shard 0
  PartWriter2 := Writer.NewPartWriter;
  WriteLn('  Part 2: ', PartWriter2.PartPath);
  
  WriteLn('Writing messages to both parts of shard 0...');
  
  // Write to first part
  for i := 0 to 2 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'FirstPart-' + IntToStr(i);
    PartWriter1.WriteMessage(Msg);
    Msg.Free;
  end;
  
  // Write to second part (same shard, different file)
  for i := 100 to 102 do
  begin
    Msg := TTestMessage.Create;
    Msg.Value := i;
    Msg.Text := 'SecondPart-' + IntToStr(i);
    PartWriter2.WriteMessage(Msg);
    Msg.Free;
  end;
  
  WriteLn('Cleaning up...');
  
  PartWriter1.Free;
  PartWriter2.Free;
  Writer.Free;
  Pattern.Free;
  
  WriteLn('Test completed successfully!');
  WriteLn('Check directory: ', TempDir);
  WriteLn('Both parts should be in shard-0000-of-0002/');
  WriteLn('');
end;

begin
  Randomize;
  
  WriteLn('Running TZioPartWriter Tests');
  WriteLn('============================');
  WriteLn('');
  
  TestMultiPartWriters;
  TestMultiplePartsInSameShard;
  
  WriteLn('============================');
  WriteLn('All tests completed!');
end.
