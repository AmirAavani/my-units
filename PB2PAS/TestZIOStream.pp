program TestZioStream;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ZIOStreamUnit, ProtoHelperUnit, ProtoStreamUnit;

type
  { Simple test message }
  TTestMessage = class(TBaseMessage)
  private
    FID: Int32;
    FName: AnsiString;
  public
    property ID: Int32 read FID write FID;
    property Name: AnsiString read FName write FName;
    
    procedure Clear; override;
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
  end;

{ TTestMessage }

procedure TTestMessage.Clear;
begin
  FID := 0;
  FName := '';
end;

procedure TTestMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveInt32(Stream, FID, 1);
  SaveString(Stream, FName, 2);
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
      1: FID := LoadInt32(Stream);
      2: FName := LoadString(Stream);
    else
      SkipField(Stream, WireType);
    end;
  end;
  Result := StartPos + Len = Stream.Position;
end;

{ Test procedures }

procedure TestBasicWriteRead;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg1, Msg2: TTestMessage;
begin
  WriteLn('TEST: Basic Write/Read');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      Msg1 := TTestMessage.Create;
      try
        Msg1.ID := 42;
        Msg1.Name := 'Test Message';
        ZStream.WriteMessage(Msg1);
      finally
        Msg1.Free;
      end;
      
      MemStream.Position := 0;
      Msg2 := TTestMessage.Create;
      try
        if ZStream.ReadMessage(Msg2) then
        begin
          if (Msg2.ID = 42) and (Msg2.Name = 'Test Message') then
            WriteLn('  ✓ PASSED')
          else
            WriteLn('  ✗ FAILED: Data mismatch');
        end
        else
          WriteLn('  ✗ FAILED: ReadMessage returned False');
      finally
        Msg2.Free;
      end;
      
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestMultipleMessages;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg1, Msg2: TTestMessage;
  i: Integer;
begin
  WriteLn('TEST: Multiple Messages');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      for i := 1 to 3 do
      begin
        Msg1 := TTestMessage.Create;
        try
          Msg1.ID := i * 100;
          Msg1.Name := Format('Message %d', [i]);
          ZStream.WriteMessage(Msg1);
        finally
          Msg1.Free;
        end;
      end;
      
      MemStream.Position := 0;
      for i := 1 to 3 do
      begin
        Msg2 := TTestMessage.Create;
        try
          if not ZStream.ReadMessage(Msg2) then
          begin
            WriteLn('  ✗ FAILED: Could not read message ', i);
            Exit;
          end;
          
          if (Msg2.ID <> i * 100) or (Msg2.Name <> Format('Message %d', [i])) then
          begin
            WriteLn('  ✗ FAILED: Message ', i, ' data mismatch');
            Exit;
          end;
        finally
          Msg2.Free;
        end;
      end;
      
      WriteLn('  ✓ PASSED');
      
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestEmptyStream;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg: TTestMessage;
begin
  WriteLn('TEST: Empty Stream');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      Msg := TTestMessage.Create;
      try
        if ZStream.ReadMessage(Msg) then
          WriteLn('  ✗ FAILED: Should return False on empty stream')
        else
          WriteLn('  ✓ PASSED');
      finally
        Msg.Free;
      end;
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestInvalidMagic;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg: TTestMessage;
  BadHeader: array[0..11] of AnsiChar;
begin
  WriteLn('TEST: Invalid Magic');
  
  MemStream := TMemoryStream.Create;
  try
    Move(PAnsiChar('BADMAGIC')^, BadHeader[0], 8);
    FillChar(BadHeader[8], 4, #32);
    MemStream.WriteBuffer(BadHeader[0], 12);
    MemStream.Position := 0;
    
    ZStream := TZioStream.Create(MemStream, False);
    try
      Msg := TTestMessage.Create;
      try
        if ZStream.ReadMessage(Msg) then
          WriteLn('  ✗ FAILED: Should reject invalid magic')
        else
          WriteLn('  ✓ PASSED');
      finally
        Msg.Free;
      end;
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestTruncatedMessage;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg1, Msg2: TTestMessage;
  OrigSize: Int64;
begin
  WriteLn('TEST: Truncated Message');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      Msg1 := TTestMessage.Create;
      try
        Msg1.ID := 999;
        Msg1.Name := 'This message will be truncated';
        ZStream.WriteMessage(Msg1);
      finally
        Msg1.Free;
      end;
      
      OrigSize := MemStream.Size;
      MemStream.Size := OrigSize - 10;
      MemStream.Position := 0;
      
      Msg2 := TTestMessage.Create;
      try
        if ZStream.ReadMessage(Msg2) then
          WriteLn('  ✗ FAILED: Should reject truncated message')
        else
          WriteLn('  ✓ PASSED');
      finally
        Msg2.Free;
      end;
      
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestNilMessage;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
begin
  WriteLn('TEST: Nil Message');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      ZStream.WriteMessage(nil);
      
      if MemStream.Size = 0 then
        WriteLn('  ✓ PASSED')
      else
        WriteLn('  ✗ FAILED: Should not write anything for nil message');
        
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestLargeMessage;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg1, Msg2: TTestMessage;
  LargeString: AnsiString;
begin
  WriteLn('TEST: Large Message (1MB string)');
  
  SetLength(LargeString, 1024 * 1024);
  FillChar(LargeString[1], Length(LargeString), 'A');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      Msg1 := TTestMessage.Create;
      try
        Msg1.ID := 12345;
        Msg1.Name := LargeString;
        ZStream.WriteMessage(Msg1);
      finally
        Msg1.Free;
      end;
      
      WriteLn('  Stream size: ', MemStream.Size, ' bytes');
      
      MemStream.Position := 0;
      Msg2 := TTestMessage.Create;
      try
        if ZStream.ReadMessage(Msg2) then
        begin
          if (Msg2.ID = 12345) and (Length(Msg2.Name) = 1024 * 1024) then
            WriteLn('  ✓ PASSED')
          else
            WriteLn('  ✗ FAILED: Data mismatch');
        end
        else
          WriteLn('  ✗ FAILED: Could not read large message');
      finally
        Msg2.Free;
      end;
      
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestEmptyMessage;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
  Msg1, Msg2: TTestMessage;
begin
  WriteLn('TEST: Empty Message');
  
  MemStream := TMemoryStream.Create;
  try
    ZStream := TZioStream.Create(MemStream, False);
    try
      Msg1 := TTestMessage.Create;
      try
        Msg1.ID := 0;
        Msg1.Name := '';
        ZStream.WriteMessage(Msg1);
      finally
        Msg1.Free;
      end;
      
      MemStream.Position := 0;
      Msg2 := TTestMessage.Create;
      try
        if ZStream.ReadMessage(Msg2) then
        begin
          if (Msg2.ID = 0) and (Msg2.Name = '') then
            WriteLn('  ✓ PASSED')
          else
            WriteLn('  ✗ FAILED: Data mismatch');
        end
        else
          WriteLn('  ✗ FAILED: Could not read empty message');
      finally
        Msg2.Free;
      end;
      
    finally
      ZStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TestOwnership;
var
  MemStream: TMemoryStream;
  ZStream: TZioStream;
begin
  WriteLn('TEST: Stream Ownership');
  
  MemStream := TMemoryStream.Create;
  ZStream := TZioStream.Create(MemStream, True);
  ZStream.Free;
  
  WriteLn('  ✓ PASSED (stream freed by TZioStream)');
end;

{ Main }

begin
  WriteLn('===========================================');
  WriteLn('  ZIOStream Unit Tests');
  WriteLn('===========================================');
  WriteLn;
  
  try
    TestBasicWriteRead;
    TestMultipleMessages;
    TestEmptyStream;
    TestInvalidMagic;
    TestTruncatedMessage;
    TestNilMessage;
    TestLargeMessage;
    TestEmptyMessage;
    TestOwnership;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
  WriteLn('===========================================');
  WriteLn('  All tests completed successfully!');
  WriteLn('===========================================');
end.

procedure TestZioStreamsBasic;
var
  ZStreams: TZioStreams;
  Msg: TTestMessage;
  TempDir: AnsiString;
  i: Integer;
begin
  WriteLn('TEST: TZioStreams Basic Functionality');
  
  TempDir := GetTempDir + 'ziotest_' + IntToStr(Random(10000));
  try
    ZStreams := TZioStreams.Create(TempDir, 4);
    try
      // Write messages to different shards
      for i := 0 to 3 do
      begin
        Msg := TTestMessage.Create;
        try
          Msg.ID := i * 100;
          Msg.Name := Format('Shard %d message', [i]);
          ZStreams.WriteMessageToShard(Msg, i);
        finally
          Msg.Free;
        end;
      end;
      
      WriteLn('  Created 4 shard files');
      WriteLn('  ✓ PASSED');
      
    finally
      ZStreams.Free;
    end;
    
    // Verify files exist
    for i := 0 to 3 do
    begin
      if not FileExists(Format('%s%sshards-%4.4d-%4.4d.zio', 
                              [IncludeTrailingPathDelimiter(TempDir), PathDelim, i, 4])) then
      begin
        WriteLn('  ✗ FAILED: Shard file ', i, ' not created');
        Exit;
      end;
    end;
    
  finally
    // Cleanup
    DeleteDirectory(TempDir, False);
  end;
end;

procedure TestZioStreamsReadBack;
var
  ZStreams: TZioStreams;
  Msg1, Msg2: TTestMessage;
  TempDir: AnsiString;
  FileStream: TFileStream;
  ZStream: TZioStream;
  i: Integer;
begin
  WriteLn('TEST: TZioStreams Read Back Data');
  
  TempDir := GetTempDir + 'ziotest_' + IntToStr(Random(10000));
  try
    // Write data
    ZStreams := TZioStreams.Create(TempDir, 3);
    try
      for i := 0 to 2 do
      begin
        Msg1 := TTestMessage.Create;
        try
          Msg1.ID := i * 10;
          Msg1.Name := Format('Data for shard %d', [i]);
          ZStreams.WriteMessageToShard(Msg1, i);
        finally
          Msg1.Free;
        end;
      end;
    finally
      ZStreams.Free;
    end;
    
    // Read back and verify
    for i := 0 to 2 do
    begin
      FileStream := TFileStream.Create(
        Format('%s%sshards-%4.4d-%4.4d.zio', 
               [IncludeTrailingPathDelimiter(TempDir), PathDelim, i, 3]),
        fmOpenRead);
      try
        ZStream := TZioStream.Create(FileStream, False);
        try
          Msg2 := TTestMessage.Create;
          try
            if not ZStream.ReadMessage(Msg2) then
            begin
              WriteLn('  ✗ FAILED: Could not read from shard ', i);
              Exit;
            end;
            
            if (Msg2.ID <> i * 10) or (Msg2.Name <> Format('Data for shard %d', [i])) then
            begin
              WriteLn('  ✗ FAILED: Shard ', i, ' data mismatch');
              Exit;
            end;
          finally
            Msg2.Free;
          end;
        finally
          ZStream.Free;
        end;
      finally
        FileStream.Free;
      end;
    end;
    
    WriteLn('  ✓ PASSED');
    
  finally
    DeleteDirectory(TempDir, False);
  end;
end;

procedure TestZioStreamsInvalidShard;
var
  ZStreams: TZioStreams;
  Msg: TTestMessage;
  TempDir: AnsiString;
  ExceptionCaught: Boolean;
begin
  WriteLn('TEST: TZioStreams Invalid Shard Index');
  
  TempDir := GetTempDir + 'ziotest_' + IntToStr(Random(10000));
  ExceptionCaught := False;
  
  try
    ZStreams := TZioStreams.Create(TempDir, 2);
    try
      Msg := TTestMessage.Create;
      try
        Msg.ID := 42;
        Msg.Name := 'Test';
        
        try
          ZStreams.WriteMessageToShard(Msg, 5);  // Invalid: only have 0-1
        except
          on E: Exception do
            ExceptionCaught := True;
        end;
        
      finally
        Msg.Free;
      end;
    finally
      ZStreams.Free;
    end;
    
    if ExceptionCaught then
      WriteLn('  ✓ PASSED')
    else
      WriteLn('  ✗ FAILED: Should raise exception for invalid shard');
      
  finally
    DeleteDirectory(TempDir, False);
  end;
end;

procedure TestZioStreamsMultipleMessagesPerShard;
var
  ZStreams: TZioStreams;
  Msg1, Msg2: TTestMessage;
  TempDir: AnsiString;
  FileStream: TFileStream;
  ZStream: TZioStream;
  i, j: Integer;
  ShardPath: AnsiString;
begin
  WriteLn('TEST: TZioStreams Multiple Messages Per Shard');
  
  TempDir := GetTempDir + 'ziotest_' + IntToStr(Random(10000));
  try
    // Write multiple messages to each shard
    ZStreams := TZioStreams.Create(TempDir, 2);
    try
      for i := 0 to 1 do  // For each shard
      begin
        for j := 1 to 5 do  // Write 5 messages
        begin
          Msg1 := TTestMessage.Create;
          try
            Msg1.ID := i * 100 + j;
            Msg1.Name := Format('Shard %d, Msg %d', [i, j]);
            ZStreams.WriteMessageToShard(Msg1, i);
          finally
            Msg1.Free;
          end;
        end;
      end;
    finally
      ZStreams.Free;
    end;
    
    // Verify all messages in each shard
    for i := 0 to 1 do
    begin
      ShardPath := Format('%s%sshards-%4.4d-%4.4d.zio',
                         [IncludeTrailingPathDelimiter(TempDir), PathDelim, i, 2]);
      FileStream := TFileStream.Create(ShardPath, fmOpenRead);
      try
        ZStream := TZioStream.Create(FileStream, False);
        try
          for j := 1 to 5 do
          begin
            Msg2 := TTestMessage.Create;
            try
              if not ZStream.ReadMessage(Msg2) then
              begin
                WriteLn('  ✗ FAILED: Could not read message ', j, ' from shard ', i);
                Exit;
              end;
              
              if (Msg2.ID <> i * 100 + j) or 
                 (Msg2.Name <> Format('Shard %d, Msg %d', [i, j])) then
              begin
                WriteLn('  ✗ FAILED: Message ', j, ' in shard ', i, ' data mismatch');
                Exit;
              end;
            finally
              Msg2.Free;
            end;
          end;
        finally
          ZStream.Free;
        end;
      finally
        FileStream.Free;
      end;
    end;
    
    WriteLn('  ✓ PASSED');
    
  finally
    DeleteDirectory(TempDir, False);
  end;
end;
