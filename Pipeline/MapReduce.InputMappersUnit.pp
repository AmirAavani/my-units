unit MapReduce.InputMappersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.NodeUnit, MapReduce.KeyValueUnit, ProtoHelperUnit;

type

  { TBaseInputMapper }

  TBaseInputMapper = class(TMapper)
  protected
    function Next(Sender: TNode): Boolean; virtual; abstract;

  public
    constructor Create;

    function Process(constref InputKey: AnsiString; InputValue: TBaseMessage;
      Sender: TNode): Boolean; override;

  end;

  { TTextInputMapper }

  TTextInputMapper = class(TBaseInputMapper)
  protected
    FText: AnsiString;
    function Next(Sender: TNode): Boolean; override;

  public
    constructor Create(constref t: AnsiString);

  end;

  { TTextStreamMapper }

  TTextStreamMapper = class(TBaseInputMapper)
  protected
    InputStream: TStream;
    Buffer: AnsiString;
    Position: Integer;

    function Next(Sender: TNode): Boolean; override;

  public
    constructor Create(constref Stream: TStream; BufferSize: Integer = 64 * 1024 * 1024);
    destructor Destroy; override;

  end;


implementation

{ TBaseInputMapper }

constructor TBaseInputMapper.Create;
begin
  inherited;

end;

function TBaseInputMapper.Process(constref InputKey: AnsiString;
  InputValue: TBaseMessage; Sender: TNode): Boolean;
begin
  while Self.Next(Sender) do;

  Result := True;
  Sender.Data.Done := True;
end;

{ TTextInputMapper }

constructor TTextInputMapper.Create(constref t: AnsiString);
begin
  inherited Create;

  FText := t;

end;

function TTextInputMapper.Next(Sender: TNode): Boolean;
begin
  WriteLn(Format('Next: %d -> %s', [ThreadID, FTExt]));
  Sender.Send(FText, nil);
  WriteLn(Format('Next: %d -> %s', [ThreadID, FTExt]));
  Result := False;

end;

{ TTextStreamMapper }

function TTextStreamMapper.Next(Sender: TNode): Boolean;
var
  ReadBytes: Integer;
  Line: AnsiString;
  PCh: PChar;

begin
  PCh := @Buffer[1];
  PCh += Position;

  Line := '';
  while Length(Buffer) <> 0 do
  begin
    while Position < Length(Buffer) do
    begin
      if PCh^ = #10 then
      begin
        Sender.Send(Line, nil);
        Exit(True);
      end;
      Line += PCh^;
      Inc(PCh);
      Inc(Position);

    end;

    ReadBytes := InputStream.Read(Buffer[1], Length(Buffer));
    Position := 0;
    PCh := @Buffer[1];
    SetLength(Buffer, ReadBytes);
  end;
  if Line <> '' then
  begin
    Sender.Send(Line, nil);
  end;


end;

constructor TTextStreamMapper.Create(constref Stream: TStream;
  BufferSize: Integer);
begin
  inherited Create;

  InputStream := Stream;
  SetLength(Buffer, BufferSize);
  Position := BufferSize;

end;

destructor TTextStreamMapper.Destroy;
begin
  InputStream.Free;
  SetLength(Buffer, 0);

  inherited Destroy;
end;


end.

