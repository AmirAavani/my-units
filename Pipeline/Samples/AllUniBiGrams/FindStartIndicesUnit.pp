unit FindStartIndicesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PipelineUnit, GenericCollection.UtilsUnit, ParamUnit;

function FindStartIndices(Task: TTask): Boolean;

implementation
uses
  TypesUnit, ALoggerUnit, SharedUnit;

type
  EEof = class(Exception);

function FindStartIndices(Task: TTask): Boolean;

  function GetNextChar(
    Reader: TStream;
    var Buffer: array of Char;
    var Index: Integer): Char;
  begin
    if Index = Length(Buffer) then
    begin
      Reader.Read(Buffer[0], Length(Buffer));
      Index := 0

    end;

    Result := Buffer[Index];
    Inc(Index);

  end;

var
  Positions: TPositionList;
  State: Integer;
  Reader, Writer: TStream;
  Size: Int64;
  Buffer: array of Char;
  i: Uint64;
  Index: Integer;
  Ch: Char;
  Start, Fin: Int64;
  Last: Boolean;
  Content: AnsiString;

const
  BufferSize = 1024 * 1024 * 8;

begin
  ALoggerUnit.GetLogger.FMTDebugLn('In FindStartIndices: "%s"', [
  ParamUnit.GetParams.InputFile.Value]);
  Reader := TFileStream.Create(
    ParamUnit.GetParams.InputFile.Value,
    fmOpenRead or fmShareDenyNone);
  Size := Reader.Size;
  ALoggerUnit.GetLogger.FMTDebugLn('Reader.Size: %d', [Size]);
  Start := ((Size + Task.ID - 1) div Task.StepInfo.NumTasks) *
       (Task.ID - 1);
  Fin := ((Size + Task.ID - 1) div Task.StepInfo.NumTasks) *
       Task.ID - 1;
  ALoggerUnit.GetLogger.FMTDebugLn('Task: %d (%d -> %d)',[Task.ID, Start, Fin]);
  Reader.Position := Start;

  Positions := TPositionList.Create;
  SetLength(Buffer, BufferSize);
  Reader.Read(Buffer[0], BufferSize);
  State := 0;
  Index := 0;
  Last := False;

  Content := '';
  for i := Start to Size do
  begin
    try
      Ch := GetNextChar(Reader, Buffer, Index);
      Content += Ch;

    except
      on e: EEof do
        Break;

    end;

    if PageTag[State + 1] = Ch then
      Inc(State)
    else
      State := 0;

    if State = Length(PageTag)  then
    begin
      Positions.Add(i - Length(PageTag));
      Content := '';
      if Positions.Count mod 10000 = 0 then
      begin
        ALoggerUnit.GetLogger.FMTDebugLn('ID: %d Index: %d Position: %d', [
          Task.ID,
          Positions.Count,
          Positions.Last]);

      end;

      State := 0;

      if Fin < i then
      begin
        Last := True;
        Break;

      end;

    end;

  end;

  if not Last then
  begin
    Positions.Add(Size);

  end;

  Reader.Free;

  ALoggerUnit.GetLogger.FMTDebugLn('ID: %d PositionsFileName: %s Count: %d', [
    Task.ID,
    GetPositionFileName(
      Task.ID,
      Task.Count),
      Positions.Count]);
  Writer := TFileStream.Create(GetPositionFileName(Task.ID, Task.Count), fmCreate);
  Positions.SaveToStream(Writer, @SaveUInt64);
  Writer.Free;
  ALoggerUnit.GetLogger.FMTDebugLn('ID: %d Positions.Count: %d', [Task.ID, Positions.Count]);
  Positions.Free;

  Result := True;

end;

end.

