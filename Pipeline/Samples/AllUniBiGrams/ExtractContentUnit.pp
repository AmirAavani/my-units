unit ExtractContentUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PipelineUnit, GenericCollection.UtilsUnit, WikiDocUnit;

function ExtractContent(Task: TTask): Boolean;

implementation

uses
  ParamUnit, TypesUnit,
  ALoggerUnit, SharedUnit, Laz2_DOM, laz2_xmlread,
  WikiParserUnit, WideStringUnit;

function ProcessData(constref Data: AnsiString): TWikiPage;
begin
  Result := nil;
  try
    if not ParseWikiNode(Data, Result) then
    begin
      Exit;

    end;

  except
    on e: EBaseWikiParser do
    begin
      FmtFatalLnIFFalse(
        False,
        'Failed in Parsing %',
        [Data]);
      raise;

    end;
    on e: EDOMError do
    begin
      Result := nil;
      ALoggerUnit.GetLogger.FMTWriteLn(
        'ERRORR: %s Data: %s',
        [e.Message, Data]);
      FmtFatalLnIFFalse(False, '%s', [e.Message]);

    end;
    on e: Exception do
    begin
      ALoggerUnit.GetLogger.FMTWriteLn('ERRORR %s', [e.Message]);
      FmtFatalLnIFFalse(False, '%s', [e.Message]);
    end;

  end;

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
  if (ParamUnit.GetParams.Pipeline.TaskID.Value <> -1) and
  (ParamUnit.GetParams.Pipeline.TasKID.Value <> Task.ID) then
  begin
    ALoggerUnit.GetLogger.FMTWriteLn('Exiting Task: %d', [Task.ID]);
    Exit(True);

  end;

  Stream := TFileStream.Create(
    GetPositionFileName(
      Task.ID,
      Task.Count),
    fmOpenRead);
  Positions := TPositionList.LoadFromStream(Stream, @LoadUInt64);
  Stream.Free;
  DebugIndex := ParamUnit.GetParams.DebugIndex.Value;
  DebugStart := ParamUnit.GetParams.DebugStart.Value;
  DebugEnd := ParamUnit.GetParams.DebugEnd.Value;
  ALoggerUnit.GetLogger.FMTWriteLn(
    'DebugIndex: %d DebugStart: %d DebugEnd: %d TaskID: %d',
    [DebugIndex, DebugStart, DebugEnd, ParamUnit.GetParams.Pipeline.TaskID.Value]);


  ALoggerUnit.GetLogger.FMTWriteLn(
    'Task.ID: %d Position.Count: %d',
    [Task.ID, Positions.Count]
  );
  Reader := TFileStream.Create(
    ParamUnit.GetParams.InputFile.Value,
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
  {ALoggerUnit.GetLogger.FMTWriteLn(
    'Task.ID: %d Start: %d Fin: %d',
    [Task.ID, Start, Fin]);
  }
  for i := 0 to Positions.Count - 2 do
  begin

    if (DebugStart <> -1) and ((i < DebugStart) or (DebugEnd < i)) then
      Continue;
    if (i <> DebugIndex) and (DebugIndex <> -1) then
      Continue;

    // ALoggerUnit.GetLogger.FMTWriteLn('*****%05d/%05d*****', [i, Positions.Count - 2]);
    // ALoggerUnit.GetLogger.FMTWriteLn('+Task.ID: %05d i:%05d', [Task.ID, i]);
    WriteLn(Format('+Task.ID: %05d i:%05d', [Task.ID, i]));
    Flush(Output);

    Reader.Position := Positions[i];
    {
    ALoggerUnit.GetLogger.FMTWriteLn(
      'Task.ID: %05d i: %05d Start: %05d Fin: %05d',
      [Task.ID, i, Positions[i], Positions[i + 1]]);
    }
    SetLength(Data, Positions[i + 1] - Positions[i]);
    ReadBytes := Reader.Read(Data[1], Positions[i + 1] - Positions[i]);
    if ReadBytes <> Positions[i + 1] - Positions[i] then
      FmtFatalLnIFFalse(
        False,
        'ReadBytes: %d Expected: %d',
        [ReadBytes, Positions[i + 1] - Positions[i]]);
    if DebugIndex <> -1 then
      ALoggerUnit.GetLogger.FMTDebugLn('Data(%d): %s',
        [Length(Data), Data]);

    try
      WikiDoc := ProcessData(Data);
      if (WikiDoc = nil) or (WikiDoc.IsADisambiguationPage) or (WikiDoc.Redirect <> '') then
      begin
        {ALoggerUnit.GetLogger.FMTWriteLn(
          '-Task.ID: %05d i:%05d',
          [Task.ID, i]);
        }
        if WikiDoc <> nil then
        begin
          {
          ALoggerUnit.GetLogger.FMTWriteLn(
            'Skipped Task.ID: %05d i: %05d %s', [
            Task.ID,
            i,
            WikiDoc.Title.ToXML('')]);
          }
        end;

        WikiDoc.Free;
        Continue;

      end;
      if DebugIndex <> -1 then
      begin
        {
        WriteLn('<B>');
        WriteLn(Format('<WikiDoc Index="%d"><Title>%s</Title>%s</WikiDoc>', [
          i, WriteAsUTF8(WikiDoc.Title), WikiDoc.ToXML]));
        WriteLn('</B>');
        }
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
         ALoggerUnit.GetLogger.FMTWriteLn('Failed in Processing Data', []);
      end;
     { on e: Exception do
      begin
        ALoggerUnit.GetLogger.FMTWriteLn('Random Errror', []);
      end;
      }
    end;
    // ALoggerUnit.GetLogger.FMTWriteLn('-Task.ID: %5d i:%5d', [Task.ID, i]);
    if WikiDoc = nil then
    begin
      LineInfo.First.Free;
      LineInfo.Second.Free;
      {WriteLn(Format('Task.ID: %d i: %d is nil',
          [Task.ID, i]));
      }
      Continue;
    end;

    {
    WriteLn(Format(
      'ID: %d i: %d Title: (%s) -> (%d, %d)', [
      Task.ID,
      i,
      WikiDoc.Title.ToXML(''),
      LineInfo.First.Count,
      LineInfo.Second.Count])
    );
    }

    {
    ALoggerUnit.GetLogger.FMTWriteLn(
      'ID: %d i: %d Title: (%s) -> (%d, %d)', [
      Task.ID,
      i,
      WikiDoc.Title.ToXML(''),
      LineInfo.First.Count,
      LineInfo.Second.Count]);;
    }
    WikiDoc.Free;

    LineInfo.First.Free;
    LineInfo.Second.Free;

    ALoggerUnit.GetLogger.FMTWriteLn('~Task.ID: %5d i:%5d', [Task.ID, i]);
    if DebugIndex <> -1 then
      Break;

  end;

  ALoggerUnit.GetLogger.FMTWriteLn('~Task.ID: %5d', [Task.ID]);
  Positions.Free;
  Reader.Free;
  UniWriter.Free;
  BiWriter.Free;

  Result := True;
end;

end.

