unit ZioStreamUnit;

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

{ We define a local class to "crack" the protected section of TBaseMessage }
type
  TBaseMessageCracker = class(TBaseMessage);

constructor TZioStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TZioStream.Destroy;
begin
  if FOwnsStream then FStream.Free;
  inherited Destroy;
end;

procedure TZioStream.WriteMessage(AMessage: TBaseMessage);
var
  TempStream: TMemoryStream;
  Writer: TProtoStreamWriter;
  // Use a string for easier assignment, or fixed array
  Header: array[0..11] of AnsiChar;
begin
  if AMessage = nil then Exit;

  // Initialize Header to avoid hints
  FillChar(Header, SizeOf(Header), #32);
  Move(PAnsiChar('ZIO1PBUF')^, Header[0], 8);

  TempStream := TMemoryStream.Create;
  try
    // Use the PUBLIC TStream version to serialize
    AMessage.SaveToStream(TempStream);

    // 1. Write ZIO Header
    FStream.WriteBuffer(Header[0], 12);

    // 2. Write Length (Varint32)
    Writer := TProtoStreamWriter.Create(FStream, False);
    try
      Writer.WriteRawVarint32(TempStream.Size);
    finally
      Writer.Free;
    end;

    // 3. Write Data
    FStream.Position := FStream.Position; // Ensure sync
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
  Magic: string[4];
begin
  Result := False;
  if AMessage = nil then
    Exit;

  // Ensure we have enough data for a header
  if FStream.Position + 12 > FStream.Size then Exit;

  // 1. Read Header
  FStream.ReadBuffer(Header[0], 12);

  // Validation
  SetLength(Magic, 4);
  Move(Header[0], Magic[1], 4);
  if Magic <> 'ZIO1' then
    Exit;

  // 2. Read Size and Data
  Reader := TProtoStreamReader.Create(FStream, False);
  try
    MsgSize := Reader.ReadVarUInt32;

    // 3. CRITICAL FIX:
    // We cast AMessage to our Cracker class to access the
    // protected LoadFromStream(TProtoStreamReader; Len: Integer)
    Result := TBaseMessageCracker(AMessage).LoadFromStream(Reader, MsgSize);
  finally
    Reader.Free;
  end;
end;

end.
