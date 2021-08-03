unit SourcerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Mapper.TypesUnit, StreamUnit;

type

  { TBaseSourcer }

  TBaseSourcer = class(TObject)
  protected
    function GetNext: Boolean; virtual; abstract;
    function GetCurrent: TKeyValue; virtual; abstract;

  public

    constructor Create;

  end;

  { TKVReader }

  TKVReader = class(TBaseSourcer)
  private
    Position: Integer;
    Stream: TMyBinStream;

  protected

  public
    constructor Create(Filename: AnsiString);
    destructor Destroy; override;

  end;

  { TNewLineReader }
  {
    TODO(Amir): This is a naive implementation! It loads the whole file in the memory.
    We really do not need this. Improve this.
  }
  TNewLineReader = class(TBaseSourcer)
  private
    WholeFile: AnsiString;
    CurrentStart, CurrentEnd: PChar;
    FSeparator: AnsiString;

  protected
    function GetNext: Boolean; override;
    function GetCurrent: TKeyValue; override;

  public
    constructor Create(Filename: AnsiString; Separator: AnsiString = sLineBreak);
    destructor Destroy; override;

  end;

implementation

uses
  StringUnit, PCharUnit;

{ TNewLineReader }

function TNewLineReader.GetNext: Boolean;
begin
  Result := CurrentEnd^ <> #0;

  CurrentStart := CurrentEnd + Length(FSeparator);
  CurrentEnd := CurrentStart;

  while (not IsPrefix(@FSeparator[1], CurrentEnd)) and (CurrentEnd^ <> #0) do
    Inc(CurrentEnd);
end;

function TNewLineReader.GetCurrent: TKeyValue;
begin
  Result.First := '';
  raise Exception.Create('');
//  Result.Second := CurrentLine;

end;

constructor TNewLineReader.Create(Filename: AnsiString; Separator: AnsiString);
var
  Stream: TMyTextStream;

begin
  inherited Create;

  Stream := TMyTextStream.Create(TFileStream.Create(Filename, fmOpenRead), True);
  FSeparator := Separator;
  WholeFile := Stream.ReadAll;
  Stream.Free;
  raise Exception.Create('');

end;

destructor TNewLineReader.Destroy;
begin

  inherited Destroy;
end;

{ TKVReader }

constructor TKVReader.Create(Filename: AnsiString);
begin
  inherited Create;

  Stream := TMyBinStream.Create(TFileStream.Create(Filename, fmOpenRead), True);
  Position := 0;

end;

destructor TKVReader.Destroy;
begin
  Stream.Free;

  inherited Destroy;
end;

{ TSource }

constructor TBaseSourcer.Create;
begin
  inherited Create;

end;

end.

