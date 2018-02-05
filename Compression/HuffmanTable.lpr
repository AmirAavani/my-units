program HuffmanTable;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fgl, UTF8Unit, HeapUnit, ParameterManagerUnit, sysutils,
  csvdocument, TextCompressionUnit
  { you can add units after this };

procedure GenerateTable;
var
  HTable: THuffmanTable;

begin
  HTable := THuffmanTable.CreateFromFiles(TRunTimeParameterManager.GetInstance.ValueByName['--RootDir'],
     '*.html', True);
  HTable.SaveToFile(TRunTimeParameterManager.GetInstance.ValueByName['--Table']);
  HTable.Free;
end;

procedure TestCompression;
var
  InputStream, OutputStream: TStream;
  Archiver: TUnicodeStringArchiver;
  HTable: THuffmanTable;
  i: Integer;
  Ch1, Ch2: Byte;

begin
  InputStream := TFileStream.Create(TRunTimeParameterManager.GetInstance.ValueByName['--InputFile'],
    fmOpenRead);
  OutputStream := TFileStream.Create('compressed.am', fmCreate);

  HTable := THuffmanTable.LoadFromFile(TRunTimeParameterManager.GetInstance.ValueByName['--Table']);
  Archiver := TUnicodeStringArchiver.Create(HTable);
  InputStream.Position := 0;
  InputStream.Position := 0;

  Archiver.Compress(InputStream, OutputStream);

  OutputStream.Free;
  InputStream.Free;

  InputStream := TFileStream.Create('compressed.am', fmOpenRead);
  OutputStream := TFileStream.Create('decompressed.am', fmCreate);

  InputStream.Position := 0;
  OutputStream.Position := 0;
  Archiver.DeCompress(InputStream, OutputStream);

  OutputStream.Free;
  InputStream.Free;

  InputStream :=  TFileStream.Create(TRunTimeParameterManager.GetInstance.ValueByName['--InputFile'],
    fmOpenRead);
  OutputStream := TFileStream.Create('decompressed.am', fmOpenRead);
  if InputStream.Size <> OutputStream.Size then
  begin
    WriteLn('Error in Size');
    Exit;
  end;
  for i := 0 to InputStream.Size - 1 do
  begin
    InputStream.ReadBuffer(Ch1, 1);
    OutputStream.ReadBuffer(Ch2, 1);
    if Ch1 <> Ch2 then
    begin
      WriteLn('Error');
      Exit;
    end;
  end;

  OutputStream.Free;
  InputStream.Free;
  HTable.Free;
end;

begin
  if TRunTimeParameterManager.GetInstance.ValueByName['--Mode'] = 'Generate' then
    GenerateTable
  else if TRunTimeParameterManager.GetInstance.ValueByName['--Mode'] = 'Compress' then
    TestCompression
  else
    WriteLn(Format('Invalid Mode: %s',
      [TRunTimeParameterManager.GetInstance.ValueByName['--Mode']]));
end.

