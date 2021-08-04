unit Mapper.LibraryUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Mapper.BaseMapperUnit, Mapper.TypesUnit, Mapper.ChannelUnit;

type

  { TNewLineReaderMapper }

  TNewLineReaderMapper = class(TBaseMapper)
  protected
    DataStream: TStream;
    CurrentData: AnsiString;
    Separator: AnsiString;

  public
    constructor Create(Stream: TStream; _Separator: AnsiString = sLineBreak);
    destructor Destroy; override;

    function Map(kv: TKeyValue; Output: TDataLine): Boolean; override;

  end;

implementation
uses
  Math, ALoggerUnit;

{ TNewLineReaderMapper }

constructor TNewLineReaderMapper.Create(Stream: TStream; _Separator: AnsiString
  );
begin
  inherited Create;

  DataStream := Stream;
  Separator := _Separator;
  CurrentData := '';

end;

destructor TNewLineReaderMapper.Destroy;
begin
  DataStream.Free;

  inherited Destroy;
end;

const
  ReadSize: Int64 = 1024 * 1024;
function TNewLineReaderMapper.Map(kv: TKeyValue; Output: TDataLine): Boolean;
var
  S: AnsiString;
  Ch: Char;
  Index: Integer;
  Count: Integer;
  LineCount: Integer;

begin
  LineCount := 0;
  Index := Pos(Separator, CurrentData);
  if Index <> 0 then
  begin
    S := Copy(CurrentData, 1, Index - 1);
    CurrentData := Copy(CurrentData, Index + Length(Separator) - 1, Length(CurrentData));
    FMTDebugLn('S = %s', [S]);
    FMTDebugLn('CurrentData = %s', [CurrentData]);

    Inc(LineCount);
    Output.Send(IntToStr(LineCount), TMapperBasicData.Create(S));
    Exit;
  end;

  SetLength(S, ReadSize);
  repeat
    Count := DataStream.Read(S[1], Length(S));
    if Count < ReadSize then
      break;


  until Pos(S, Separator) = 0;

end;

end.

