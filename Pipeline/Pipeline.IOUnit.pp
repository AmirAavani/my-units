unit Pipeline.IOUnit;

{$mode objfpc}{$H+}

interface

uses
  StreamUnit, ProtoHelperUnit, Classes, SysUtils;

type

  { TPipelineWriter }

  TPipelineWriter = class(TMyBinStream)
  private
    FFilename: AnsiString;

  public
    property Filename: AnsiString read FFilename;

    constructor Create(AFilename: AnsiString);
    destructor Destroy; override;

    procedure WriteProto(aProto: TBaseMessage); virtual;

  end;

implementation

{ TPipelineWriter }

constructor TPipelineWriter.Create(AFilename: AnsiString);
begin
  inherited Create(TFileStream.Create(AFilename, fmCreate), True);

  FFilename := AFilename;
  if DirectoryExists(AFilename, True) then
    RemoveDir(AFilename);
  MkDir(AFilename);

end;

destructor TPipelineWriter.Destroy;
begin

  inherited Destroy;
end;

procedure TPipelineWriter.WriteProto(aProto: TBaseMessage);
var
  Output: TBytesStream;

begin
  Output := TBytesStream.Create;
  aProto.SaveToStream(Output);

  Self.WriteStr(AnsiString(Output.Bytes));

  Output.Free;

end;

end.

