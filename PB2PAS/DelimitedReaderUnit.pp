unit DelimitedReaderUnit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, BufStream, Generics.Collections,
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

      function ReadMessage(out AMessage: TMessage): boolean;
      function ReadMessage(out AKey: ansistring; out AMessage: TMessage): boolean;
    end;

    TShardReaders = specialize TObjectList<TShardReader>;

  private
    FPattern: TPattern;
    FBufferSize: integer;
    FShards: TShardReaders;

    function GetShardReader(Index: integer): TShardReader;
  public
    constructor Create(aPattern: TPattern; ABufferSize: integer = 65536);
    destructor Destroy; override;

    property ShardReader[Index: integer]: TShardReader read GetShardReader;

    // Reads the next message from a specific shard. Returns False if EOF is reached.
    function ReadMessageFromShard(ShardIndex: integer; out AMessage: TMessage): boolean;
    function ReadMessageFromShard(ShardIndex: integer; out AKey: ansistring;
      out AMessage: TMessage): boolean;
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

  // fmShareDenyWrite allows reading while writers might still be operating elsewhere,
  // though typically reading happens after map/reduce phases are complete.
  FFileStream := TFileStream.Create(FFiles[FCurrentFileIndex],
    fmOpenRead or fmShareDenyWrite);
  FBufferedStream := TReadBufStream.Create(FFileStream, FBufferSize);

  Inc(FCurrentFileIndex);
  Result := True;
end;

procedure TDelimitedReader.TShardReader.CloseCurrentFile;
begin
  if Assigned(FBufferedStream) then
    FreeAndNil(FBufferedStream);
  if Assigned(FFileStream) then
    FreeAndNil(FFileStream);
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

function TDelimitedReader.TShardReader.ReadMessage(out AMessage: TMessage): boolean;
var
  MsgSize: uint64;
  TempStream: TMemoryStream;
begin
  AMessage := nil;
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

  // Create message instance and read the defined number of bytes
  TempStream := TMemoryStream.Create;
  try
    if MsgSize > 0 then
    begin
      TempStream.SetSize(MsgSize);
      if FBufferedStream.Read(TempStream.Memory^, MsgSize) < MsgSize then
        raise EStreamException.Create('Unexpected EOF while reading message payload');
    end;

    TempStream.Position := 0;

    // Requires constructor Create; virtual; on TBaseMessage
    AMessage := TMessage.Create;
    try
      AMessage.LoadFromStream(TempStream);
      Result := True;
    except
      AMessage.Free;
      AMessage := nil;
      raise;
    end;
  finally
    TempStream.Free;
  end;
end;

{ TDelimitedReader.TShardReader }

function TDelimitedReader.TShardReader.ReadMessage(out AKey: ansistring;
  out AMessage: TMessage): boolean;

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
  AMessage := nil;
  Result := False;

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
      CloseCurrentFile;
  end;

  // 2. Read the entire wrapper block into memory for safe parsing
  WrapperStream := TMemoryStream.Create;
  try
    if WrapperSize > 0 then
    begin
      WrapperStream.SetSize(WrapperSize);
      if FBufferedStream.Read(WrapperStream.Memory^, WrapperSize) < WrapperSize then
        raise EStreamException.Create('Unexpected EOF while reading wrapper payload');
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
          try
            MsgStream.SetSize(FieldLen);
            if FieldLen > 0 then
              WrapperStream.ReadBuffer(MsgStream.Memory^, FieldLen);
            MsgStream.Position := 0;

            AMessage := TMessage.Create;
            AMessage.LoadFromStream(MsgStream);
          finally
            MsgStream.Free;
          end;
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

    // Protobuf 3 semantics: If the message field was totally empty/missing,
    // it implies a default empty message should exist.
    if not Assigned(AMessage) then
      AMessage := TMessage.Create;

    Result := True;
  finally
    WrapperStream.Free;
  end;
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
  FShards.Free;
  // Note: We don't free FPattern here assuming it was passed in and owned by the caller.
  // If TDelimitedReader owns it, add FPattern.Free;
  inherited Destroy;
end;

function TDelimitedReader.GetShardReader(Index: integer): TShardReader;
begin
  if (Index < 0) or (Index >= FShards.Count) then
    raise EShardException.CreateFmt('Shard index %d out of bounds', [Index]);
  Result := FShards[Index];
end;

function TDelimitedReader.ReadMessageFromShard(ShardIndex: integer;
  out AMessage: TMessage): boolean;
begin
  Result := GetShardReader(ShardIndex).ReadMessage(AMessage);
end;

function TDelimitedReader.ReadMessageFromShard(ShardIndex: integer;
  out AKey: ansistring; out AMessage: TMessage): boolean;
begin
  Result := GetShardReader(ShardIndex).ReadMessage(AKey, AMessage);

end;

end.
