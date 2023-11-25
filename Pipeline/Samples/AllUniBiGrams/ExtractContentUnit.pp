unit ExtractContentUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PipelineUnit, GenericCollection.UtilsUnit, WikiDocUnit;

function ExtractContent(Task: TTask): Boolean;

implementation

uses
  ParameterManagerUnit, TypesUnit,
  ALoggerUnit, SharedUnit, Laz2_DOM, laz2_xmlread,
  WikiParserUnit, WideStringUnit;

function ProcessData(constref Data: AnsiString): TWikiPage;
var
  Doc: TXMLDocument;
  S: TStream;

begin
  Doc := nil;
  Result := nil;
  S := TStringStream.Create(Data);
  try
    ReadXMLFile(Doc, S);

  except
    on e: Exception do
    begin
      S.Free;
      Exit;

    end;

  end;
  if (Doc = nil) or (Doc.FirstChild = nil) then
    Exit;

  try
    Result := ParseWiki(Doc.FirstChild);

  except
    on e: EBaseWikiParser do
    begin
      FmtFatalLnIFFalse(
        False,
        'Failed in Parsing %',
        [Doc.FirstChild.TextContent]);
      S.Free;
      Doc.Free;
      raise;

    end;
    on e: EDOMError do
    begin
      Result := nil;
      S.Free;
      Doc.Free;
      ALoggerUnit.GetLogger.FMTDebugLn(
        'ERRORR: %s Data: %s',
        [e.Message, Data]);
      FmtFatalLnIFFalse(False, '%s', [e.Message]);

    end;
    on e: Exception do
    begin
      ALoggerUnit.GetLogger.FMTDebugLn('ERRORR %s', [e.Message]);
      FmtFatalLnIFFalse(False, '%s', [e.Message]);
    end;

  end;

  S.Free;
  Doc.Free;

end;

function IsEmptyString(constref Str: WideString): Boolean;
begin
  Result := Str = ' ';

end;

function ExtractContent(Task: TTask): Boolean;
var
  Positions: TPositionList;
  Stream: TFileStream;
  Reader, UniWriter, BiWriter: TStream;
  i: Integer;
  Size: Int64;
  Start, Fin: Int64;
  Data: AnsiString;
  ReadBytes: Integer;
  WikiDoc: TWikiPage;
  DebugIndex, DebugStart, DebugEnd: Int64;
  LineInfo: TWideStringListPair;

begin
  if (GetRunTimeParameterManager.ValueByName['--TaskID'].AsIntegerOrDefault(-1) <> -1) and
  (GetRunTimeParameterManager.ValueByName['--TaskID'].AsIntegerOrDefault(-1) <> Task.ID) then
  begin
    ALoggerUnit.GetLogger.FMTDebugLn('Exiting Task: %d', [Task.ID]);
    Exit(True);

  end;

  Stream := TFileStream.Create(
    GetPositionFileName(
      Task.ID,
      Task.Count),
    fmOpenRead);
  Positions := TPositionList.LoadFromStream(Stream, @LoadUInt64);
  Stream.Free;
  DebugIndex := GetRunTimeParameterManager.ValueByName['--DebugIndex'].AsIntegerOrDefault(-1);
  DebugStart := GetRunTimeParameterManager.ValueByName['--DebugStart'].AsIntegerOrDefault(-1);
  DebugEnd := GetRunTimeParameterManager.ValueByName['--DebugEnd'].AsIntegerOrDefault(-1);
  ALoggerUnit.GetLogger.FMTDebugLn(
    'DebugIndex: %d DebugStart: %d DebugEnd: %d',
    [DebugIndex, DebugStart, DebugEnd]);


  ALoggerUnit.GetLogger.FMTDebugLn(
    'Task.ID: %d Position.Count: %d',
    [Task.ID, Positions.Count]
  );
  Reader := TFileStream.Create(
    GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString,
    fmOpenRead or fmShareDenyNone);
  UniWriter := TFileStream.Create(
    GetExtractUnigramsFileName(
      Task.ID,
      Task.Count),
    fmCreate);
  BiWriter := TFileStream.Create(
    GetExtractBigramsFileName(
      Task.ID,
      Task.Count),
    fmCreate);

  Size := Reader.Size;
  Start := ((Size + Task.ID - 1) div Task.StepInfo.NumTasks) *
       (Task.ID - 1);
  Fin := ((Size + Task.ID - 1) div Task.StepInfo.NumTasks) *
       Task.ID - 1;
  ALoggerUnit.GetLogger.FMTDebugLn(
    'Task.ID: %d Start: %d Fin: %d',
    [Task.ID, Start, Fin]);

  for i := 0 to Positions.Count - 2 do
  begin
    if (DebugStart <> -1) and ((i < DebugStart) or (DebugEnd < i)) then
      Continue;
    if (i <> DebugIndex) and (DebugIndex <> -1) then
      Continue;

    ALoggerUnit.GetLogger.FMTDebugLn('*****%05d/%05d*****', [i, Positions.Count - 2], -1);
    ALoggerUnit.GetLogger.FMTDebugLn('+Task.ID: %05d i:%05d', [Task.ID, i], -1);

    Reader.Position := Positions[i];
    ALoggerUnit.GetLogger.FMTDebugLn(
      'Task.ID: %05d i: %05d Start: %05d Fin: %05d',
      [Task.ID, i, Positions[i], Positions[i + 1]], -1);

    SetLength(Data, Positions[i + 1] - Positions[i]);
    ReadBytes := Reader.Read(Data[1], Positions[i + 1] - Positions[i]);
    if ReadBytes <> Positions[i + 1] - Positions[i] then
      FmtFatalLnIFFalse(
        False,
        'ReadBytes: %d Expected: %d',
        [ReadBytes, Positions[i + 1] - Positions[i]]);
    if DebugIndex <> -1 then
      WriteLn(Format('Data(%d): %s',
        [Length(Data), Data]));

    try
      WikiDoc := ProcessData(Data);
      if (WikiDoc = nil) or (WikiDoc.IsADisambiguationPage) or (WikiDoc.Redirect <> nil) then
      begin
        ALoggerUnit.GetLogger.FMTDebugLn(
          '-Task.ID: %05d i:%05d',
          [Task.ID, i],
          -1);
        if WikiDoc <> nil then
        begin
          ALoggerUnit.GetLogger.FMTDebugLn(
            'Skipped Task.ID: %05d i: %05d %s', [
            Task.ID,
            i,
            WikiDoc.Title.ToXML('')], -1);
        end;

        WikiDoc.Free;
        Continue;

      end;
      if DebugIndex <> -1 then
      begin
        WriteLn('<B>');
        WriteLn(Format('<WikiDoc Index="%d"><Title>%s</Title>%s</WikiDoc>', [
          i, WikiDoc.Title.ToXML('  '), WikiDoc.ToXML]));
        WriteLn('</B>');

      end;
      LineInfo := WikiDoc.ExportText;
      LineInfo.First.RemoveAllValuesMatching(@IsEmptyString);

      LineInfo.First.SaveToStream(UniWriter, @SaveWideString);
      LineInfo.Second.SaveToStream(BiWriter, @SaveWideString);

      if DebugIndex <> -1 then
      begin
        WriteLn(Format('--: %d, %d', [LineInfo.First.Count, LineInfo.Second.Count]));
        WriteLn(Format('Unigrams: %s', [WriteAsUTF8(LineInfo.First.JoinStrings())]));
        WriteLn(Format('Bigrams: %s', [WriteAsUTF8(LineInfo.Second.JoinStrings())]));

        LineInfo.First.Free;
        LineInfo.Second.Free;

        WikiDoc.Free;
        UniWriter.Free;
        biWriter.Free;
        Positions.Free;

        Exit;

      end;

    except
      on e: EBaseWikiParser do
      begin
         ALoggerUnit.GetLogger.FMTDebugLn('Failed in Processing Data', []);
      end;
      on e: Exception do
      begin
        ALoggerUnit.GetLogger.FMTDebugLn('Random Errror', []);
      end;
    end;
    ALoggerUnit.GetLogger.FMTDebugLn('-Task.ID: %5d i:%5d', [Task.ID, i], -1);
    if WikiDoc = nil then
    begin
      LineInfo.First.Free;
      LineInfo.Second.Free;
        WriteLn(Format('Task.ID: %d i: %d is nil',
          [Task.ID, i]));
      Continue;
    end;

    ALoggerUnit.GetLogger.FMTDebugLn(
      'ID: %d i: %d Title: (%s) -> (%d, %d)', [
      Task.ID,
      i,
      WikiDoc.Title.ToXML(''),
      LineInfo.First.Count,
      LineInfo.Second.Count], -1);

    WikiDoc.Free;

    LineInfo.First.Free;
    LineInfo.Second.Free;

    ALoggerUnit.GetLogger.FMTDebugLn('~Task.ID: %5d i:%5d', [Task.ID, i], -1);
    if DebugIndex <> -1 then
      Break;

  end;

  ALoggerUnit.GetLogger.FMTDebugLn('~Task.ID: %5d', [Task.ID], -1);
  Positions.Free;
  Reader.Free;
  UniWriter.Free;
  BiWriter.Free;

  Result := True;
end;

end.

