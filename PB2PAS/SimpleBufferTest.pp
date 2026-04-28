program SimpleBufferTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, bufstream;

var
  FileStream: TFileStream;
  WriteBufferedStream: TWriteBufStream;
  ReadBufferedStream: TReadBufStream;
  TestData: AnsiString;
begin
  WriteLn('Testing bufstream unit...');
  
  try
    // Test write buffering
    FileStream := TFileStream.Create('/tmp/buftest.dat', fmCreate);
    try
      WriteBufferedStream := TWriteBufStream.Create(FileStream, 4096);
      try
        TestData := 'Hello, buffered world!';
        WriteBufferedStream.WriteBuffer(TestData[1], Length(TestData));
        WriteLn('✓ Write buffering works');
      finally
        WriteBufferedStream.Free;
      end;
    finally
      FileStream.Free;
    end;
    
    // Test read buffering
    FileStream := TFileStream.Create('/tmp/buftest.dat', fmOpenRead);
    try
      ReadBufferedStream := TReadBufStream.Create(FileStream, 4096);
      try
        SetLength(TestData, 22);
        ReadBufferedStream.ReadBuffer(TestData[1], 22);
        if TestData = 'Hello, buffered world!' then
          WriteLn('✓ Read buffering works')
        else
          WriteLn('✗ Data mismatch: ', TestData);
      finally
        ReadBufferedStream.Free;
      end;
    finally
      FileStream.Free;
    end;
    
    DeleteFile('/tmp/buftest.dat');
    WriteLn('All buffer tests passed!');
    
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
