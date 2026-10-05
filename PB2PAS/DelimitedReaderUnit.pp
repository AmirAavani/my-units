unit DelimitedReaderUnit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, BufStream, Generics.Collections,
  PatternUnit, ProtoHelperUnit;

type
  { TDelimitedReader }
  generic TDelimitedReader<TMessage: TBaseMessage> = class(TObject)
  public type

    { TShardReader }
    TShardReader = class(TObject)
    private
      FShardIndex: Integer;
      FShardPath: string;
      FFiles: TStringList;
      FCurrentFileIndex: Integer;

      FFileStream: TFileStream;
      FBufferedStream: TReadBufStream;
      FBufferSize: Integer;

      function OpenNextFile: Boolean;
      procedure CloseCurrentFile;
      function ReadVarint(out Value: UInt64): Boolean;
    public
      constructor Create(AShardIndex: Integer; const AShardPath: string; ABufferSize: Integer);
      destructor Destroy; override;

      function ReadMessage(out AMessage: TMessage): Boolean;
    end;

    TShardReaders = specialize TObjectList<TShardReader>;

  private
    FPattern: TPattern;
    FBufferSize: Integer;
    FShards: TShardReaders;

    function GetShardReader(Index: Integer): TShardReader;
  public
    constructor Create(aPattern: TPattern; ABufferSize: Integer = 65536);
    destructor Destroy; override;

    property ShardReader[Index: Integer]: TShardReader read GetShardReader;

    // Reads the next message from a specific shard. Returns False if EOF is reached.
    function ReadMessageFromShard(ShardIndex: Integer; out AMessage: TMessage): Boolean;
  end;

implementation

{ TDelimitedReader.TShardReader }

constructor TDelimitedReader.TShardReader.Create(AShardIndex: Integer;
  const AShardPath: string; ABufferSize: Integer);
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

function TDelimitedReader.TShardReader.OpenNextFile: Boolean;
begin
  CloseCurrentFile;

  if FCurrentFileIndex >= FFiles.Count then
    Exit(False); // No more files in this shard

  // fmShareDenyWrite allows reading while writers might still be operating elsewhere,
  // though typically reading happens after map/reduce phases are complete.
  FFileStream := TFileStream.Create(FFiles[FCurrentFileIndex], fmOpenRead or fmShareDenyWrite);
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

function TDelimitedReader.TShardReader.ReadVarint(out Value: UInt64): Boolean;
var
  B: Byte;
  Shift: Integer;
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
        raise EStreamException.Create('Unexpected EOF while reading varint length prefix');
    end;

    Value := Value or (UInt64(B and $7F) shl Shift);
    if (B and $80) = 0 then
      Break;

    Inc(Shift, 7);
    if Shift >= 64 then
      raise EStreamException.Create('Malformed varint: exceeds 64 bits');
  end;

  Result := True;
end;

function TDelimitedReader.TShardReader.ReadMessage(out AMessage: TMessage): Boolean;
var
  MsgSize: UInt64;
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


{ TDelimitedReader }

constructor TDelimitedReader.Create(aPattern: TPattern; ABufferSize: Integer);
var
  i: Integer;
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

function TDelimitedReader.GetShardReader(Index: Integer): TShardReader;
begin
  if (Index < 0) or (Index >= FShards.Count) then
    raise EShardException.CreateFmt('Shard index %d out of bounds', [Index]);
  Result := FShards[Index];
end;

function TDelimitedReader.ReadMessageFromShard(ShardIndex: Integer; out AMessage: TMessage): Boolean;
begin
  Result := GetShardReader(ShardIndex).ReadMessage(AMessage);
end;

end.
