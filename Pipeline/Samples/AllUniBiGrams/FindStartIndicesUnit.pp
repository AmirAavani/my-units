unit FindStartIndicesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PipelineUnit, GenericCollection.UtilsUnit;

function FindStartIndices(Task: TTask): Boolean;

implementation
uses
  ParameterManagerUnit, TypesUnit,
  StreamUnit, Math, ALoggerUnit, OnceUnit, SharedUnit;

type
  EEof = class(Exception);

function FindStartIndices(Task: TTask): Boolean;

  function GetNextChar(
    Reader: TStream;
    var Buffer: AnsiString;
    var Index: Integer): Char;
  begin
    if Index = Length(Buffer) then
    begin
      Index := 0;
      Reader.Read(Buffer[1], Length(Buffer));

    end;

    Result := Buffer[Index];
    Inc(Index);

  end;

var
  Positions: TPositionList;
  State: Integer;
  Reader, Writer: TStream;
  Size: Int64;
  Buffer: AnsiString;
  i: Uint64;
  Index: Integer;
  Ch: Char;
  Start, Fin: Int64;
  Last: Boolean;

const
  BufferSize = 1024 * 1024 * 8;

begin
  FMTDebugLn('In FindStartIndices: "%s"', [
  GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString]);
  Reader := TFileStream.Create(
    GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString,
    fmOpenRead or fmShareDenyNone);
  Size := Reader.Size;
  FMTDebugLn('Reader.Size: %d', [Size]);
  Start := ((Size + Task.ID - 1) div Task.StepInfo.NumTasks) *
       (Task.ID - 1);
  Fin := ((Size + Task.ID - 1) div Task.StepInfo.NumTasks) *
       Task.ID - 1;
  FMTDebugLn('Task: %d (%d -> %d)',[Task.ID, Start, Fin]);
  Reader.Position := Start;

  Positions := TPositionList.Create;
  SetLength(Buffer, BufferSize);
  Reader.Read(Buffer[1], BufferSize);
  State := 0;
  Index := 0;
  Last := False;

  for i := Start to Size do
  begin
    try
      Ch := GetNextChar(Reader, Buffer, Index);

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
      if Positions.Count mod 10000 = 0 then
      begin
        FMTDebugLn('ID: %d Position: %d', [Task.ID, Positions.Last]);

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

  FMTDebugLn('ID: %d PositionsFileName: %s', [
    Task.ID,
    GetPositionFileName(
      Task.ID,
      Task.Count)]);
  Writer := TFileStream.Create(GetPositionFileName(Task.ID, Task.Count), fmCreate);
  Positions.SaveToStream(Writer, @SaveUInt64);
  Writer.Free;
  FMTDebugLn('ID: %d Positions.Count: %d', [Task.ID, Positions.Count]);
  Positions.Free;

  Result := True;

end;

end.

