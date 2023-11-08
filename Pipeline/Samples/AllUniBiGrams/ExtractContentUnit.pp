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
  S := TStringStream.Create(Data);
  try
    ReadXMLFile(Doc, S);
    Result := ParseWiki(Doc.FirstChild);

  except
    on e: EBaseWikiParser do
    begin
      FMTDebugLn('Failed in Parsing %', [Doc.FirstChild.TextContent]);
      S.Free;
      Doc.Free;
      raise;

    end;
    on e: EDOMError do
    begin
      Result := nil;
      S.Free;
      Doc.Free;
      FMTDebugLn('Error: %s Data: %s', [e.Message, Data])

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
  FMTDebugLn('DebugIndex: %d DebugStart: %d DebugEnd: %d', [DebugIndex, DebugStart, DebugEnd]);


  FMTDebugLn(
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
  FMTDebugLn('Task.ID: %d Start: %d Fin: %d', [Task.ID, Start, Fin]);

  for i := 0 to Positions.Count - 2 do
  begin
    if (DebugStart <> -1) and ((i < DebugStart) or (DebugEnd < i)) then
      Continue;
    if (i <> DebugIndex) and (DebugIndex <> -1) then
      Continue;

    FMTDebugLn('*****%d/%d*****', [i, Positions.Count - 2]);
    FMTDebugLn('+Task.ID: %5d i:%5d', [Task.ID, i]);

    Reader.Position := Positions[i];
    FMTDebugLn(
      'Task.ID: %d i: %d Start: %d Fin: %d',
      [Task.ID, i, Positions[i], Positions[i + 1]]);

    SetLength(Data, Positions[i + 1] - Positions[i]);
    ReadBytes := Reader.Read(Data[1], Positions[i + 1] - Positions[i]);
    if ReadBytes <> Positions[i + 1] - Positions[i] then
      FmtFatalLn('ReadBytes: %d Expected: %d', [ReadBytes, Positions[i + 1] - Positions[i]]);
    if DebugIndex <> -1 then
      FMTDebugLn('Data(%d): %s',
        [Length(Data), Data],
        1);

    try
      WikiDoc := ProcessData(Data);
      LineInfo := WikiDoc.ExportText;
      LineInfo.First.RemoveAllValuesMatching(@IsEmptyString);

      LineInfo.First.SaveToStream(UniWriter, @SaveWideString);
      LineInfo.Second.SaveToStream(BiWriter, @SaveWideString);

      if DebugIndex <> -1 then
      begin
        WriteLn('<B>');
        WriteLn(Format('<WikiDoc Index="%d"><Title>%s</Title>%s</WikiDoc>', [
          i, WikiDoc.Title.ToXML('  '), WikiDoc.ToXML]));
        WriteLn('</B>');


        FMTDebugLn('Unigrams: %s', [WriteAsUTF8(LineInfo.First.JoinStrings())]);
        FMTDebugLn('Bigrams: %s', [WriteAsUTF8(LineInfo.Second.JoinStrings())]);

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
         FMTDebugLn('Failed in Processing Data', []);
       end;
    end;
    FMTDebugLn('-Task.ID: %5d i:%5d', [Task.ID, i]);
    if WikiDoc = nil then
    begin
        WriteLn(Format('Task.ID: %d i: %d is nil',
          [Task.ID, i]));
      Continue;
    end;

    FMTDebugLn('ID: %d i: %d Title: %s', [Task.ID, i, WikiDoc.Title.ToXML('')]);

    LineInfo := WikiDoc.ExportText;
    WikiDoc.Free;
    LineInfo.First.RemoveAllValuesMatching(@IsEmptyString);

    //FMTDebugLn('Unigrams: %s', [WriteAsUTF8(LineInfo.First.JoinStrings())]);
    //FMTDebugLn('Bigrams: %s', [WriteAsUTF8(LineInfo.Second.JoinStrings())]);
    LineInfo.First.Free;
    LineInfo.Second.Free;

    FMTDebugLn('~Task.ID: %5d i:%5d', [Task.ID, i]);
    if DebugIndex <> -1 then
      Break;

  end;

  Positions.Free;
  Reader.Free;
  UniWriter.Free;
  BiWriter.Free;

  Result := True;
end;

end.

