unit ZIOStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoStreamUnit, ProtoHelperUnit;

type

  { TZioStream }

  TZioStream = class(TObject)
  private
    FStream: TStream;
    FOwnsStream: Boolean;

  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;

    procedure WriteMessage(AMessage: TBaseMessage);
    function ReadMessage(AMessage: TBaseMessage): Boolean;

    property Stream: TStream read FStream;
  end;

implementation

type
  { Cracker class to access protected LoadFromStream }
  TBaseMessageCracker = class(TBaseMessage);

{ TZioStream }

constructor TZioStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;

end;

destructor TZioStream.Destroy;
begin
  if FOwnsStream then
    FStream.Free;

  inherited Destroy;

end;

procedure TZioStream.WriteMessage(AMessage: TBaseMessage);
var
  TempStream: TMemoryStream;
  Writer: TProtoStreamWriter;
  Header: array[0..11] of AnsiChar;
begin
  if AMessage = nil then
    Exit;

  FillChar(Header, SizeOf(Header), #32);
  Move(PAnsiChar('ZIO1PBUF')^, Header[0], 8);

  TempStream := TMemoryStream.Create;
  try
    AMessage.SaveToStream(TempStream);

    FStream.WriteBuffer(Header[0], 12);

    Writer := TProtoStreamWriter.Create(FStream, False);
    try
      Writer.WriteRawVarint32(TempStream.Size);
    finally
      Writer.Free;
    end;

    FStream.WriteBuffer(TempStream.Memory^, TempStream.Size);

  finally
    TempStream.Free;
  end;
end;

function TZioStream.ReadMessage(AMessage: TBaseMessage): Boolean;
var
  Reader: TProtoStreamReader;
  Header: array[0..11] of AnsiChar;
  MsgSize: UInt32;
  Magic: string[8];
begin
  Result := False;

  // Check header bounds
  if (AMessage = nil) or (FStream.Position + 12 > FStream.Size) then
    Exit;

  FStream.ReadBuffer(Header[0], 12);

  // Validate full 8-byte magic
  SetLength(Magic, 8);
  Move(Header[0], Magic[1], 8);
  if Magic <> 'ZIO1PBUF' then
    Exit;
  Reader := TProtoStreamReader.Create(FStream, False);
  try
    MsgSize := Reader.ReadVarUInt32;

    // Check message bounds
    if FStream.Position + MsgSize > FStream.Size then
      Exit;

    Result := TBaseMessageCracker(AMessage).LoadFromStream(Reader, MsgSize);
  finally
    Reader.Free;
  end;

end;

end.

