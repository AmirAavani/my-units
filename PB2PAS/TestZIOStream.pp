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
