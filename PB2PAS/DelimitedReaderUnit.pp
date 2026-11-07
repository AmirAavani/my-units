unit DelimitedReaderUnit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, BufStream, FastListUnit,
  PatternUnit, ProtoHelperUnit;

type
  { TDelimitedReader }
  generic TDelimitedReader<TMessage: TBaseMessage> = class(TObject)
  public
  type

    { TShardReader }
    TShardReader = class(TObject)
    private
      FShardIndex: integer;
      FShardPath: string;
      FFiles: TStringList;
      FCurrentFileIndex: integer;

      FFileStream: TFileStream;
      FBufferedStream: TReadBufStream;
      FBufferSize: integer;

      function OpenNextFile: boolean;
      procedure CloseCurrentFile;
      function ReadVarint(out Value: uint64): boolean;
    public
      constructor Create(AShardIndex: integer; const AShardPath: string;
        ABufferSize: integer);
      destructor Destroy; override;

      function ReadMessage(var AMessage: TMessage): boolean;
      function ReadMessage(out AKey: rawbytestring; var AMessage: TMessage): boolean;
    end;

    TShardReaders = specialize TFastObjectList<TShardReader>;

  private
    FPattern: TPattern;
    FBufferSize: integer;
    FShards: TShardReaders;

    function GetShardReader(Index: integer): TShardReader;
  public
    property Shards: TShardReaders read FShards;

    constructor Create(aPattern: TPattern; ABufferSize: integer = 65536);
    destructor Destroy; override;

    property ShardReader[Index: integer]: TShardReader read GetShardReader;

    // Reads the next message from a specific shard. Returns False if EOF is reached.
    function ReadMessageFromShard(ShardIndex: integer; var AMessage: TMessage): boolean;
    function ReadMessageFromShard(ShardIndex: integer; out AKey: rawbytestring;
      var AMessage: TMessage): boolean;
  end;

implementation

{ TDelimitedReader.TShardReader }

constructor TDelimitedReader.TShardReader.Create(AShardIndex: integer;
  const AShardPath: string; ABufferSize: integer);
var
  SR: TSearchRec;
  SearchPath: string;
begin
  inherited Create;
  FShardIndex := AShardIndex;
  FShardPath := IncludeTrailingPathDelimiter(AShardPath);
  FBufferSize := ABufferSize;
  FFiles := TStringList.Create;
  FCurrentFileIndex := 0;

  // Find all part files in the shard directory
  SearchPath := FShardPath + 'part-*.pb';
  if FindFirst(SearchPath, faAnyFile - faDirectory, SR) = 0 then
  begin
    repeat
      FFiles.Add(FShardPath + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  // Sorting alphabetically guarantees chronological/sequential order
  // because of the part-<timestamp>-<writerid>-<sequence>.pb naming convention
  FFiles.Sort;
end;

destructor TDelimitedReader.TShardReader.Destroy;
begin
  CloseCurrentFile;
  FFiles.Free;
  inherited Destroy;
end;

function TDelimitedReader.TShardReader.OpenNextFile: boolean;
begin
  CloseCurrentFile;

  if FCurrentFileIndex >= FFiles.Count then
    Exit(False); // No more files in this shard

  FFileStream := TFileStream.Create(FFiles[FCurrentFileIndex],
    fmOpenRead or fmShareDenyWrite);
  FBufferedStream := TReadBufStream.Create(FFileStream, FBufferSize);

  // CRITICAL: Prevent TReadBufStream from automatically freeing FFileStream
  // behind our back, avoiding double-free crashes in CloseCurrentFile.
  FBufferedStream.SourceOwner := False;

  Inc(FCurrentFileIndex);
  Result := True;
end;

procedure TDelimitedReader.TShardReader.CloseCurrentFile;
begin
  // 1. Freeing the buffered stream first
  if Assigned(FBufferedStream) then
    FreeAndNil(FBufferedStream);

  // 2. Safely close and release the underlying file stream handle
  if Assigned(FFileStream) then
    FreeAndNil(FFileStream);
end;

function TDelimitedReader.TShardReader.ReadMessage(var AMessage: TMessage): boolean;
var
  MsgSize: uint64;
  TempStream: TMemoryStream;
begin
  Result := False;

  while True do
  begin
    // If no stream is open, try to open the next file
    if not Assigned(FBufferedStream) then
    begin
      if not OpenNextFile then
        Exit(False); // All files processed
    end;

    // Try to read the Varint size prefix
    if ReadVarint(MsgSize) then
      Break // Successfully found a message!
    else
      CloseCurrentFile; // Hit EOF on current file, loop around and open the next one
  end;

  // Create message instance and read the defined number of bytes with exception safety
  TempStream := TMemoryStream.Create;
  if MsgSize > 0 then
  begin
    TempStream.SetSize(MsgSize);
    if FBufferedStream.Read(TempStream.Memory^, MsgSize) < MsgSize then
      raise EStreamException.Create('Unexpected EOF while reading message payload');
  end;

  TempStream.Position := 0;

  AMessage.LoadFromStream(TempStream);
  Result := True;
  TempStream.Free; // Guaranteed cleanup even if LoadFromStream raises an exception
end;

function TDelimitedReader.TShardReader.ReadVarint(out Value: uint64): boolean;
var
  B: byte;
  Shift: integer;
begin
  Value := 0;
  Shift := 0;
  Result := False;

  while True do
  begin
    if FBufferedStream.Read(B, 1) = 0 then
    begin
      if Shift = 0 then
        Exit(False) // Clean EOF right between messages
      else
        raise EStreamException.Create(
          'Unexpected EOF while reading varint length prefix');
    end;

    Value := Value or (uint64(B and $7F) shl Shift);
    if (B and $80) = 0 then
      Break;

    Inc(Shift, 7);
    if Shift >= 64 then
      raise EStreamException.Create('Malformed varint: exceeds 64 bits');
  end;

  Result := True;
end;

function TDelimitedReader.TShardReader.ReadMessage(out AKey: rawbytestring;
  var AMessage: TMessage): boolean;

  function ReadVarintFromStream(Stream: TStream; out Value: uint64): boolean;
  var
    B: byte;
    Shift: integer;
  begin
    Value := 0;
    Shift := 0;
    Result := False;

    while Stream.Read(B, 1) = 1 do
    begin
      Value := Value or (uint64(B and $7F) shl Shift);
      if (B and $80) = 0 then Exit(True);

      Inc(Shift, 7);
      if Shift >= 64 then
        raise EStreamException.Create('Malformed varint: exceeds 64 bits');
    end;
  end;

var
  WrapperSize, FieldLen: uint64;
  TagInfo: uint64;
  WireType: integer;
  WrapperStream, MsgStream: TMemoryStream;
begin
  AKey := '';
  Result := False;

  if Assigned(FBufferedStream) then
  begin
    if FBufferedStream.Position >= FBufferedStream.Size then
    begin
      CloseCurrentFile;
    end;
  end;

  // 1. Find the next message wrapper size
  while True do
  begin
    if not Assigned(FBufferedStream) then
    begin
      if not OpenNextFile then
        Exit(False);
    end;

    if ReadVarint(WrapperSize) then
      Break
    else
      CloseCurrentFile; // Hit EOF on current file, loop around and open the next one
  end;

  // 2. Read the entire wrapper block into memory for safe parsing
  WrapperStream := TMemoryStream.Create;
  if WrapperSize > 0 then
  begin
    WrapperStream.SetSize(WrapperSize);
    if FBufferedStream.Read(WrapperStream.Memory^, WrapperSize) < WrapperSize then
      raise EStreamException.Create(
        Format('Unexpected EOF while reading wrapper payload: %d', [WrapperSize]));
  end;
  WrapperStream.Position := 0;

  // 3. Parse the Protobuf Wrapper tags
  while WrapperStream.Position < WrapperStream.Size do
  begin
    if not ReadVarintFromStream(WrapperStream, TagInfo) then
      Break;

    WireType := TagInfo and 7;

    case TagInfo of
      $0A: // Field 1 (Key): Tag 1, WireType 2 (Length-Delimited)
      begin
        ReadVarintFromStream(WrapperStream, FieldLen);
        SetLength(AKey, FieldLen);
        if FieldLen > 0 then
          WrapperStream.ReadBuffer(AKey[1], FieldLen);
      end;

      $12: // Field 2 (Message): Tag 2, WireType 2 (Length-Delimited)
      begin
        ReadVarintFromStream(WrapperStream, FieldLen);
        MsgStream := TMemoryStream.Create;
        MsgStream.SetSize(FieldLen);
        if FieldLen > 0 then
          WrapperStream.ReadBuffer(MsgStream.Memory^, FieldLen);
        MsgStream.Position := 0;

        AMessage.LoadFromStream(MsgStream);
        MsgStream.Free; // Guaranteed cleanup even if LoadFromStream fails
      end;
      else
        // Skip unknown fields forwards to ensure forwards-compatibility
        case WireType of
          0: ReadVarintFromStream(WrapperStream, FieldLen); // Varint
          1: WrapperStream.Seek(8, soCurrent);              // 64-bit
          2: // Length-delimited block
          begin
            ReadVarintFromStream(WrapperStream, FieldLen);
            WrapperStream.Seek(FieldLen, soCurrent);
          end;
          5: WrapperStream.Seek(4, soCurrent);              // 32-bit
          else
            raise EStreamException.Create('Invalid wire type inside wrapper');
        end;
    end;
  end;

  if Assigned(FBufferedStream) then
  begin
    if FBufferedStream.Position >= FBufferedStream.Size then
      CloseCurrentFile;
  end;

  Result := True;
  WrapperStream.Free; // Guaranteed cleanup for the wrapper stream
end;

{ TDelimitedReader }

constructor TDelimitedReader.Create(aPattern: TPattern; ABufferSize: integer);
var
  i: integer;
begin
  inherited Create;

  FPattern := TPattern.Create(aPattern.BasePath, aPattern.NumShards);
  FBufferSize := ABufferSize;
  FShards := TShardReaders.Create(True); // Owns instances
  FShards.Capacity := FPattern.NumShards;

  // Eagerly initialize readers for all shards.
  // If a shard directory doesn't exist or is empty, it just gets an empty file list.
  for i := 0 to FPattern.NumShards - 1 do
    FShards.Add(TShardReader.Create(i, FPattern.GetShardPath(i), FBufferSize));
end;

destructor TDelimitedReader.Destroy;
begin
  FPattern.Free;
  FShards.Free;

  inherited Destroy;
end;

function TDelimitedReader.GetShardReader(Index: integer): TShardReader;
begin
  Result := FShards[Index];
end;

function TDelimitedReader.ReadMessageFromShard(ShardIndex: integer;
  var AMessage: TMessage): boolean;
begin
  Result := FShards[ShardIndex].ReadMessage(AMessage);
end;

function TDelimitedReader.ReadMessageFromShard(ShardIndex: integer;
  out AKey: rawbytestring; var AMessage: TMessage): boolean;
begin
  Result := FShards[ShardIndex].ReadMessage(AKey, AMessage);

end;

end.
