unit ZIOStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bufstream, ProtoStreamUnit, ProtoHelperUnit,
  Generics.Collections, DateUtils;

type
  TIntList = specialize TList<integer>;
  TAnsiStringArray = array of ansistring;

  { Exception Classes }

  EZIOStreamException = class(Exception);

  EPatternException = class(EZIOStreamException);

  EShardException = class(EZIOStreamException);

  EStreamException = class(EZIOStreamException);

  { TPattern - Encapsulates shard path pattern }

  TPattern = class(TObject)
  private
    FBasePath: ansistring;
    FNumShards: integer;
    FRemainder: integer;
    FModulo: integer;

    function GetFilteredNumShards: integer;

  public
    constructor Create(const Pattern: ansistring);
    constructor Create(const ABasePath: ansistring; ANumShards: integer);
    constructor Create(const Pattern: TPattern);
    destructor Destroy; override;

    function GetShardPath(ShardIndex: integer): ansistring;

    { Create a filtered pattern that only includes shards where (shard_index mod ModuloValue) = Remainder }
    function WithModulo(Remainder, ModuloValue: integer): TPattern;

    property BasePath: ansistring read FBasePath;
    property NumShards: integer read GetFilteredNumShards;
  end;

  { TZioStream }
  TZioStream = class(TObject)
  private
    FFilePath: ansistring;
    FIsWriter: boolean;

    // Writer fields
    FFileStream: TFileStream;
    FBuffer: TMemoryStream;
    FMaxBufferSize: integer;

    // Reader fields
    FReadStream: TReadBufStream;

    procedure EnsureWriterIsValid;
    procedure EnsureReaderIsValid;
    procedure FlushBufferToDisk;
  public
    constructor CreateWriter(const AFilePath: ansistring; ABufferSize: integer = 65536);
    constructor CreateReader(const AFilePath: ansistring; ABufferSize: integer = 131072);
    destructor Destroy; override;

    procedure WriteMessage(AMessage: TBaseMessage);
    function ReadMessage(AMessage: TBaseMessage): boolean;

    // Forces any remaining data in the buffer to disk
    procedure Flush;
  end;


  { TZioStream }
  TZioStreamList = specialize TList<TZioStream>;
  TAnsiStringList = specialize TList<ansistring>;

  { TZioReader - Generic ZIO reader }

  generic TZioReader<T: TBaseMessage> = class(TObject)
  protected
  type
    { TZioShardReader - Manages reading from one shard (multiple parts) }
    TZioShardReader = class(TObject)
    private
      FPartPaths: TStringList;        // All part paths for this shard (sorted)
      FCurrentPartIndex: integer;     // Which part we're currently reading
      FCurrentStream: TZioStream;     // Currently open part stream
      FBufferSize: integer;

      procedure OpenNextPart;
      procedure CloseCurrentPart;

    public
      constructor Create(const AShardDir: ansistring; ABufferSize: integer);
      destructor Destroy; override;

      function ReadMessage(AMessage: TBaseMessage): boolean;
      function HasMoreParts: boolean;
    end;

    TZioShardReaderList = specialize TObjectList<TZioShardReader>;

  private
    FShardReaders: TZioShardReaderList;
    FCurrentShardIndex: integer;
    FPattern: TPattern;
    FBufferSize: integer;

  protected
    function GetTotalStreams: integer;

  public
    property NumShards: integer read GetTotalStreams;
    constructor Create(const APaths: array of ansistring;
      ABufferSize: integer = 131072); overload;
    constructor Create(APattern: TPattern; ABufferSize: integer = 131072); overload;
    destructor Destroy; override;

    function ReadMessage(var AMessage: T): boolean;
    function ReadMessageFromShard(ShardIndex: integer; var AMessage: T): boolean;
  end;

  { TZioWriter - Generic ZIO writer }

  generic TZioWriter<T: TBaseMessage> = class(TObject)
  public
  type
    { Forward declaration }
    TZioShardWriter = class;

    { TZioPartWriter - Writes to a single part file }
    TZioPartWriter = class(TObject)
    private
      FZioStream: TZioStream;
      FPartPath: ansistring;
    public
      constructor Create(const APartPath: ansistring; ABufferSize: integer = 65536);
      destructor Destroy; override;

      procedure WriteMessage(AMessage: T);

      property PartPath: ansistring read FPartPath;
    end;

    { TZioShardWriter - Manages writing to one specific shard }
    TZioShardWriter = class(TObject)
    private
      FParentWriter: TZioWriter;  // Back reference (not owned)
      FShardIndex: integer;
      FSequenceCounter: integer;
      FCurrentPartWriter: TZioPartWriter;  // Reused for WriteMessage
      FBufferSize: integer;

      function GeneratePartPath: ansistring;
      procedure EnsureShardDirectoryExists;

    public
      constructor Create(AParentWriter: TZioWriter; AShardIndex: integer;
        ABufferSize: integer);
      destructor Destroy; override;

      { Creates a new part writer - caller owns and must free }
      function NewPartWriter: TZioPartWriter;

      { Convenience: writes to current part (creates if needed, reuses) }
      procedure WriteMessage(AMessage: T);

      property ShardIndex: integer read FShardIndex;
    end;

    TZioShardWriterList = specialize TObjectList<TZioShardWriter>;
    TZioPartWriterList = specialize TObjectList<TZioPartWriter>;

  private
    FPattern: TPattern;
    FBufferSize: integer;
    FCurrentShardIndex: integer;  // For round-robin in NewPartWriter
    FShardWriters: TZioShardWriterList;  // Lazily created shard writers

    function GetShardDirectoryPath(ShardIndex: integer): ansistring;
    function GetTotalStreams: integer;

  public
    property NumShards: integer read GetTotalStreams;
    constructor Create(APattern: TPattern; ABufferSize: integer = 65536);
    destructor Destroy; override;

    { Get shard writer for specific shard (lazily created) }
    function GetShardWriter(ShardIndex: integer): TZioShardWriter; inline;

    { Convenience: write message to specific shard }
    procedure WriteMessageToShard(AMessage: T; ShardIndex: integer); inline;

  end;


implementation

uses
  BaseUnix;

function IsFileHandleValid(AStream: THandleStream): boolean;
var
  Flags: cint;
begin
  if AStream = nil then Exit(False);
  Flags := fpfcntl(AStream.Handle, F_GETFL);
  Result := (Flags <> -1);
end;


type
  { Cracker class to access protected LoadFromStream }
  TBaseMessageCracker = class(TBaseMessage);

  { TPattern }

constructor TPattern.Create(const Pattern: ansistring);
var
  AtPos: integer;
  NumShardsStr: ansistring;
begin
  inherited Create;

  // Parse "path@N" format
  AtPos := Pos('@', Pattern);
  if AtPos = 0 then
    raise EPatternException.CreateFmt(
      'Invalid pattern format: "%s". Expected "path@numShards"', [Pattern]);

  FBasePath := Copy(Pattern, 1, AtPos - 1);
  NumShardsStr := Copy(Pattern, AtPos + 1, Length(Pattern));

  if not TryStrToInt(NumShardsStr, FNumShards) then
    raise EPatternException.CreateFmt('Invalid number of shards: "%s"', [NumShardsStr]);

  if FNumShards <= 0 then
    raise EPatternException.CreateFmt('Number of shards must be positive: %d',
      [FNumShards]);

  FRemainder := 0;
  FModulo := 1;
end;

constructor TPattern.Create(const ABasePath: ansistring; ANumShards: integer);
begin
  inherited Create;

  FBasePath := ABasePath;
  FNumShards := ANumShards;
  FRemainder := 0;
  FModulo := 1;


  if FNumShards <= 0 then
    raise EPatternException.CreateFmt('Number of shards must be positive: %d',
      [FNumShards]);
end;

constructor TPattern.Create(const Pattern: TPattern);
begin
  inherited Create;

  FBasePath := Pattern.FBasePath;
  FModulo := Pattern.FModulo;
  FNumShards := Pattern.NumShards;
  FRemainder := Pattern.FRemainder;
end;

destructor TPattern.Destroy;
begin
  inherited Destroy;
end;

function TPattern.GetFilteredNumShards: integer;
var
  i, Count: integer;
begin
  if FModulo = 1 then
    Exit(FNumShards);

  // Count how many shards match the filter
  Count := 0;
  for i := 0 to FNumShards - 1 do
  begin
    if i mod FModulo = FRemainder then
      Inc(Count);
  end;
  Result := Count;
end;

function TPattern.GetShardPath(ShardIndex: integer): ansistring;
var
  ActualShardIndex, i, Count: integer;
begin
  // TODO(Amir): This is a naive implementation! Improve this.
  Count := 0;
  ActualShardIndex := -1;
  for i := 0 to FNumShards - 1 do
  begin
    if i mod FModulo <> FRemainder then
      Continue;

    if Count = ShardIndex then
    begin
      ActualShardIndex := i;
      Break;
    end;
    Inc(Count);
  end;

  if ActualShardIndex = -1 then
    raise EShardException.CreateFmt(
      'Shard index %d out of range [0..%d] for filtered pattern',
      [ShardIndex, GetFilteredNumShards - 1]);

  // Return directory path for multi-part shards
  Result := Format('%sshard-%4.4d-of-%4.4d',
    [IncludeTrailingPathDelimiter(FBasePath), ActualShardIndex, FNumShards]);
end;

function TPattern.WithModulo(Remainder, ModuloValue: integer): TPattern;
begin
  Result := TPattern.Create(FBasePath, FNumShards);
  Result.FRemainder := Remainder;
  Result.FModulo := ModuloValue;
end;

{ TZioStream }

constructor TZioStream.CreateWriter(const AFilePath: ansistring; ABufferSize: integer);
begin
  inherited Create;
  FIsWriter := True;
  FFilePath := AFilePath;
  FMaxBufferSize := ABufferSize;

  FBuffer := TMemoryStream.Create;
  FBuffer.SetSize(FMaxBufferSize + 1024); // Pre-allocate to prevent resizing overhead
  FBuffer.Clear;

  // Lazy initialization: We don't open the file until the first flush
  FFileStream := nil;
end;

constructor TZioStream.CreateReader(const AFilePath: ansistring; ABufferSize: integer);
var
  Header: array[0..11] of ansichar;
  Magic: string[8];
begin
  inherited Create;
  FIsWriter := False;
  FFilePath := AFilePath;
  FMaxBufferSize := ABufferSize;

  FFileStream := TFileStream.Create(FFilePath, fmOpenRead or fmShareDenyWrite);
  FReadStream := TReadBufStream.Create(FFileStream, FMaxBufferSize);

  // Read and validate the ZIO header ONCE when the file is opened
  if FReadStream.Size >= 12 then
  begin
    FReadStream.ReadBuffer(Header[0], 12);
    SetLength(Magic, 8);
    Move(Header[0], Magic[1], 8);

    if Magic <> 'ZIO1PBUF' then
      raise EStreamException.CreateFmt('Invalid ZIO file format in "%s"', [FFilePath]);
  end
  else
    raise EStreamException.CreateFmt('File "%s" is too small to be a ZIO file',
      [FFilePath]);
end;

destructor TZioStream.Destroy;
begin
  if FIsWriter then
  begin
    FlushBufferToDisk; // Ensure final data is written before destruction
    if FFileStream <> nil then FFileStream.Free;
    FBuffer.Free;
  end
  else
  begin
    if FReadStream <> nil then FReadStream.Free;
    if FFileStream <> nil then FFileStream.Free;
  end;

  inherited Destroy;
end;

procedure TZioStream.EnsureWriterIsValid;
var
  Mode: word;
  IsNewFile: boolean;
  Header: array[0..11] of ansichar;
begin
  if (FFileStream <> nil) and IsFileHandleValid(FFileStream) then
    Exit;

  if FFileStream <> nil then
  begin
    FFileStream.Free;
    FFileStream := nil;
  end;

  IsNewFile := not FileExists(FFilePath);

  if IsNewFile then
    Mode := fmCreate or fmShareDenyWrite
  else
    Mode := fmOpenReadWrite or fmShareDenyWrite;

  FFileStream := TFileStream.Create(FFilePath, Mode);

  if IsNewFile then
  begin
    // Write the ZIO header ONCE when the file is created
    FillChar(Header, SizeOf(Header), #32);
    Move(pansichar('ZIO1PBUF')^, Header[0], 8);
    FFileStream.WriteBuffer(Header[0], 12);
  end
  else
  begin
    // If appending, seek to the end
    FFileStream.Position := FFileStream.Size;
  end;
end;

procedure TZioStream.EnsureReaderIsValid;
var
  LogicalPosition: int64;
begin
  // If the stream is open and the OS says the handle is healthy, we are good!
  if (FFileStream <> nil) and IsFileHandleValid(FFileStream) then
    Exit;

  if FFileStream <> nil then
  begin
    // Save the exact logical byte position we are currently at
    LogicalPosition := FReadStream.Position;

    // Free the dead streams
    FReadStream.Free;
    FFileStream.Free;

    // Resurrect the streams
    FFileStream := TFileStream.Create(FFilePath, fmOpenRead or fmShareDenyWrite);
    FReadStream := TReadBufStream.Create(FFileStream, FMaxBufferSize);

    // Seek back to where we were before macOS killed the handle
    FReadStream.Position := LogicalPosition;
  end;
end;

procedure TZioStream.FlushBufferToDisk;
begin
  if not FIsWriter or (FBuffer.Size = 0) then
    Exit;

  // 1. Ensure the OS file descriptor is alive and ready
  EnsureWriterIsValid;

  // 2. Write the raw memory block directly to disk
  FFileStream.WriteBuffer(FBuffer.Memory^, FBuffer.Size);

  // 3. Clear the buffer (resets Size to 0, keeps Capacity)
  FBuffer.Clear;
end;

procedure TZioStream.Flush;
begin
  FlushBufferToDisk;
end;

procedure TZioStream.WriteMessage(AMessage: TBaseMessage);
var
  TempStream: TMemoryStream;
  Writer: TProtoStreamWriter;
begin
  if not FIsWriter or (AMessage = nil) then Exit;

  TempStream := TMemoryStream.Create;
  AMessage.SaveToStream(TempStream);

  Writer := TProtoStreamWriter.Create(FBuffer, False);
  // Write ONLY the VarInt size
  Writer.WriteRawVarint32(TempStream.Size);
  Writer.Free;

  // Write ONLY the Protobuf data
  FBuffer.WriteBuffer(TempStream.Memory^, TempStream.Size);

  TempStream.Free;

  if FBuffer.Size >= FMaxBufferSize then
    FlushBufferToDisk;
end;

function TZioStream.ReadMessage(AMessage: TBaseMessage): boolean;
var
  Reader: TProtoStreamReader;
  MsgSize: uint32;
begin
  Result := False;
  if FIsWriter or (AMessage = nil) then Exit;

  EnsureReaderIsValid;

  // If we are at the end of the file, return False gracefully
  if FReadStream.Position >= FReadStream.Size then Exit;

  Reader := TProtoStreamReader.Create(FReadStream, False);
  // Read ONLY the VarInt size
  MsgSize := Reader.ReadVarUInt32;

  if FReadStream.Position + MsgSize > FReadStream.Size then Exit;

  // Read ONLY the Protobuf data
  Result := TBaseMessageCracker(AMessage).LoadFromStream(Reader, MsgSize);
  Reader.Free;
end;

{ TZioReader.TZioShardReader }

constructor TZioReader.TZioShardReader.Create(const AShardDir: ansistring;
  ABufferSize: integer);
var
  SearchRec: TSearchRec;
  PartPath: ansistring;
begin
  inherited Create;

  FPartPaths := TStringList.Create;
  FCurrentPartIndex := 0;
  FCurrentStream := nil;
  FBufferSize := ABufferSize;

  // Find all *.zio files in shard directory
  if FindFirst(AShardDir + PathDelim + '*.zio', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        PartPath := AShardDir + PathDelim + SearchRec.Name;
        FPartPaths.Add(PartPath);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Sort alphabetically (part-0000-xxx.zio, part-0001-xxx.zio, ...)
  FPartPaths.Sort;

  // Open first part if available
  if FPartPaths.Count > 0 then
    OpenNextPart;
end;

destructor TZioReader.TZioShardReader.Destroy;
begin
  CloseCurrentPart;
  FPartPaths.Free;
  inherited Destroy;
end;

procedure TZioReader.TZioShardReader.OpenNextPart;
var
  PartPath: ansistring;
begin
  CloseCurrentPart;

  if FCurrentPartIndex < FPartPaths.Count then
  begin
    PartPath := FPartPaths[FCurrentPartIndex];
    FCurrentStream := TZioStream.CreateReader(PartPath, FBufferSize);
  end;
end;


procedure TZioReader.TZioShardReader.CloseCurrentPart;
begin
  if FCurrentStream <> nil then
  begin
    FCurrentStream.Free;
    FCurrentStream := nil;
  end;
end;

function TZioReader.TZioShardReader.ReadMessage(AMessage: TBaseMessage): boolean;
begin
  Result := False;

  // Try reading from current part
  if FCurrentStream <> nil then
  begin
    Result := FCurrentStream.ReadMessage(AMessage);

    if Result then
      Exit; // Successfully read message

    // Current part exhausted, try next part
    Inc(FCurrentPartIndex);
    if HasMoreParts then
    begin
      OpenNextPart;
      Result := ReadMessage(AMessage); // Recursive call for next part
    end;
  end;
end;

function TZioReader.TZioShardReader.HasMoreParts: boolean;
begin
  Result := FCurrentPartIndex < FPartPaths.Count;
end;

{ TZioReader }

constructor TZioReader.Create(const APaths: array of ansistring;
  ABufferSize: integer = 131072);
var
  i: integer;
  ShardReader: TZioShardReader;
  Path: ansistring;
begin
  inherited Create;

  FShardReaders := TZioShardReaderList.Create(True); // Owns objects
  FCurrentShardIndex := 0;
  FBufferSize := ABufferSize;
  FPattern := nil;

  // Create shard readers for each path (assuming directories)
  for i := 0 to High(APaths) do
  begin
    Path := APaths[i];

    if not DirectoryExists(Path) then
      raise EStreamException.CreateFmt('Shard directory not found: %s', [Path]);

    ShardReader := TZioShardReader.Create(Path, FBufferSize);
    FShardReaders.Add(ShardReader);
  end;
end;

constructor TZioReader.Create(APattern: TPattern; ABufferSize: integer = 131072);
var
  i: integer;
  ShardReader: TZioShardReader;
  ShardDir: ansistring;
begin
  inherited Create;

  FShardReaders := TZioShardReaderList.Create(True); // Owns objects
  FCurrentShardIndex := 0;
  FPattern := TPattern.Create(APattern);
  FBufferSize := ABufferSize;

  // Create shard readers for each shard
  for i := 0 to APattern.NumShards - 1 do
  begin
    ShardDir := APattern.GetShardPath(i);

    if not DirectoryExists(ShardDir) then
      raise EStreamException.CreateFmt('Shard directory not found: %s', [ShardDir]);

    ShardReader := TZioShardReader.Create(ShardDir, FBufferSize);
    FShardReaders.Add(ShardReader);
  end;
end;

destructor TZioReader.Destroy;
begin
  FPattern.Free;
  FShardReaders.Free; // Automatically frees all shard readers
  inherited Destroy;
end;

function TZioReader.ReadMessage(var AMessage: T): boolean;
begin
  Result := False;

  // Read from shards sequentially
  while FCurrentShardIndex < FShardReaders.Count do
  begin
    Result := FShardReaders[FCurrentShardIndex].ReadMessage(AMessage);

    if Result then
      Exit; // Successfully read a message

    // Current shard exhausted, move to next
    Inc(FCurrentShardIndex);
  end;

  // All shards exhausted
  Result := False;
end;

function TZioReader.ReadMessageFromShard(ShardIndex: integer; var AMessage: T): boolean;
begin
  Result := False;

  // Validate shard index
  if (ShardIndex < 0) or (ShardIndex >= FShardReaders.Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)',
      [ShardIndex, FShardReaders.Count - 1]);

  Result := FShardReaders[ShardIndex].ReadMessage(AMessage);
end;

function TZioReader.GetTotalStreams: integer;
begin
  Result := FShardReaders.Count;
end;

{ TZioWriter.TZioShardWriter }

constructor TZioWriter.TZioShardWriter.Create(AParentWriter: TZioWriter;
  AShardIndex: integer; ABufferSize: integer);
begin
  inherited Create;

  FParentWriter := AParentWriter;
  FShardIndex := AShardIndex;
  FSequenceCounter := 0;
  EnsureShardDirectoryExists;
  FCurrentPartWriter := NewPartWriter;
  FBufferSize := ABufferSize;
end;

destructor TZioWriter.TZioShardWriter.Destroy;
begin
  FCurrentPartWriter.Free;

  inherited Destroy;
end;

function TZioWriter.TZioShardWriter.GeneratePartPath: ansistring;
var
  ShardDir: ansistring;
  Timestamp: int64;
begin
  ShardDir := FParentWriter.GetShardDirectoryPath(FShardIndex);

  // Get current Unix timestamp in milliseconds
  Timestamp := DateTimeToUnix(Now, False) * 1000 + MilliSecondOf(Now);

  // Format: part-{seq:04d}-{timestamp}.zio for alphabetical sorting
  Result := Format('%s%spart-%4.4d-%d.zio', [ShardDir, PathDelim,
    FSequenceCounter, Timestamp]);

  Inc(FSequenceCounter);
end;

procedure TZioWriter.TZioShardWriter.EnsureShardDirectoryExists;
var
  ShardDir: ansistring;
begin
  ShardDir := FParentWriter.GetShardDirectoryPath(FShardIndex);
  if not DirectoryExists(ShardDir) then
    ForceDirectories(ShardDir);
end;

function TZioWriter.TZioShardWriter.NewPartWriter: TZioPartWriter;
var
  PartPath: ansistring;
begin
  EnsureShardDirectoryExists;
  PartPath := GeneratePartPath;
  Result := TZioPartWriter.Create(PartPath, FBufferSize);
end;

procedure TZioWriter.TZioShardWriter.WriteMessage(AMessage: T);
begin
  if AMessage = nil then
    Exit;

  FCurrentPartWriter.WriteMessage(AMessage);
end;

{ TZioWriter.TZioPartWriter }

constructor TZioWriter.TZioPartWriter.Create(const APartPath: ansistring;
  ABufferSize: integer = 65536);
var
  FileStream: TFileStream;
begin
  inherited Create;

  FPartPath := APartPath;

  FZioStream := TZioStream.CreateWriter(APartPath, ABufferSize);
end;

destructor TZioWriter.TZioPartWriter.Destroy;
begin
  FZioStream.Free;
  inherited Destroy;
end;

procedure TZioWriter.TZioPartWriter.WriteMessage(AMessage: T);
begin
  if AMessage = nil then
    Exit;
  FZioStream.WriteMessage(AMessage);
end;

{ TZioWriter }

constructor TZioWriter.Create(APattern: TPattern; ABufferSize: integer = 65536);
var
  ShardIndex: integer;
begin
  inherited Create;

  FPattern := TPattern.Create(APattern);
  FBufferSize := ABufferSize;
  FCurrentShardIndex := 0;
  FShardWriters := TZioShardWriterList.Create(True);  // Owns shard writers
  FShardWriters.Count := FPattern.NumShards;
  for ShardIndex := 0 to FPattern.NumShards - 1 do
    FShardWriters[ShardIndex] := TZioShardWriter.Create(Self, ShardIndex, FBufferSize);

end;

destructor TZioWriter.Destroy;
begin
  FShardWriters.Free;
  FPattern.Free;
  inherited Destroy;
end;

function TZioWriter.GetShardDirectoryPath(ShardIndex: integer): ansistring;
var
  ActualShardIndex, i, Count: integer;
begin
  // Handle filtered patterns (with modulo)
  Count := 0;
  ActualShardIndex := -1;
  for i := 0 to FPattern.FNumShards - 1 do
  begin
    if i mod FPattern.FModulo <> FPattern.FRemainder then
      Continue;

    if Count = ShardIndex then
    begin
      ActualShardIndex := i;
      Break;
    end;
    Inc(Count);
  end;

  if ActualShardIndex = -1 then
    raise EShardException.CreateFmt('Shard index %d out of range [0..%d]',
      [ShardIndex, FPattern.NumShards - 1]);

  Result := Format('%sshard-%4.4d-of-%4.4d',
    [IncludeTrailingPathDelimiter(FPattern.BasePath), ActualShardIndex,
    FPattern.FNumShards]);
end;

function TZioWriter.GetShardWriter(ShardIndex: integer): TZioShardWriter;
begin
  Result := FShardWriters[ShardIndex];
end;

procedure TZioWriter.WriteMessageToShard(AMessage: T; ShardIndex: integer);
var
  ShardWriter: TZioShardWriter;
begin
  if AMessage = nil then
    Exit;

  ShardWriter := GetShardWriter(ShardIndex);
  ShardWriter.WriteMessage(AMessage);
end;

function TZioWriter.GetTotalStreams: integer;
begin
  Result := FPattern.NumShards;
end;

end.
