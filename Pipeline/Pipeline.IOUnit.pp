unit Pipeline.IOUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, StreamUnit, ProtoHelperUnit,  GenericCollectionUnit, Pipeline.TypesUnit,
  QueueUnit;

type

  { TPipelineWriter }

  TPipelineWriter = class(specialize TCollection<TMyBinStream>)
  private
    FFilenames: TAnsiStringList;

  public
    property Filenames: TAnsiStringList read FFilenames;

    // Takes ownership of Filesnames
    constructor Create(_Filenames: TAnsiStringList);
    destructor Destroy; override;

    procedure WriteProto(aProto: TBaseMessage); virtual;
    procedure WriteUInt64(v: UInt64); virtual;
    procedure WriteInt64(v: Int64); virtual;
    procedure WriteDouble(v: Double); virtual;
    procedure WriteSingle(v: Single); virtual;
    procedure WriteAnsiString(v: AnsiString); virtual;

  end;

implementation
uses
  PathHelperUnit, ALoggerUnit, SimpleTypesUnit;

{ TPipelineWriter }

constructor TPipelineWriter.Create(_Filenames: TAnsiStringList);
var
  AFilename: AnsiString;
  NowTS: Integer;

begin
  inherited Create;

  NowTS := DateTimeToTimeStamp(Now).Time;
  FFilenames := Filenames;
  for AFilename in FFilenames do
  begin
    if DirectoryExists(AFilename, True) then
      RemoveDir(AFilename);
    MkDir(AFilename);
    Self.Add(TMyBinStream.Create(TFileStream.Create(JoinPath(AFilename, IntToStr(NowTS)), fmCreate), True));

  end;

  FmtFatalLnIFFalse(FFilenames.Count <> 1, 'TPipelineWriter does not support writing in multiple files, yet!', []);

end;

destructor TPipelineWriter.Destroy;
begin
  FFilenames.Free;

  inherited Destroy;
end;

procedure TPipelineWriter.WriteProto(aProto: TBaseMessage);
var
  Output: TBytesStream;

begin
  Output := TBytesStream.Create;
  aProto.SaveToStream(Output);

  Self[0].WriteStr(AnsiString(Output.Bytes));

  Output.Free;

end;

procedure TPipelineWriter.WriteUInt64(v: UInt64);
var
  p: TUInt64Message;

begin
  p := TuInt64Message.Create;
  p.Data := v;

  Self.WriteProto(p);

  p.Free;

end;

procedure TPipelineWriter.WriteInt64(v: Int64);
var
  p: TInt64Message;

begin
  p := TInt64Message.Create;
  p.Data := v;

  Self.WriteProto(p);

  p.Free;

end;

procedure TPipelineWriter.WriteDouble(v: Double);
var
  p: TDoubleMessage;

begin
  p := TDoubleMessage.Create;
  p.Data := v;

  Self.WriteProto(p);

  p.Free;

end;

procedure TPipelineWriter.WriteSingle(v: Single);
var
  p: TFloatMessage;

begin
  p := TFloatMessage.Create;
  p.Data := v;

  Self.WriteProto(p);

  p.Free;
end;

procedure TPipelineWriter.WriteAnsiString(v: AnsiString);
var
  p: TStringMessage;

begin
  p := TStringMessage.Create;
  p.Data := v;

  Self.WriteProto(p);

  p.Free;

end;

end.

