unit DelimitedProtoStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, bufstream, ProtoStreamUnit, ProtoHelperUnit,
  Generics.Collections, DateUtils;

type
  TIntList = specialize TList<integer>;
  TAnsiStringArray = array of ansistring;

  { Exception Classes }

  EDelimitedProtoStreamException = class(Exception);

  EPatternException = class(EDelimitedProtoStreamException);

  EShardException = class(EDelimitedProtoStreamException);

  EStreamException = class(EDelimitedProtoStreamException);

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

  { TDelimitedProtoStream }
  { 
    Low-level stream for reading/writing delimited Protocol Buffer messages.
    
    Format: Each message is written as:
      [VarInt32: message_size][Protobuf bytes: message_size bytes]
      
    Writer Mode:
      - Messages are serialized to a temporary stream to calculate exact size
      - VarInt32 size prefix is written to the in-memory buffer
      - Protobuf bytes are appended immediately after the size
      - Buffer is flushed to disk when it reaches FMaxBufferSize threshold
      - File handle is created lazily on first flush (not on constructor)
      - If file handle becomes invalid (e.g., macOS sleep), it's automatically recovered
      
    Reader Mode:
      - Uses TReadBufStream for OS-level buffering (reduces syscalls)
      - Reads VarInt32 size, then reads exact message_size bytes
      - Returns False gracefully on EOF (no exceptions)
      - File handle recovery on macOS file descriptor invalidation
      
    Thread Safety: NOT thread-safe. Use one instance per thread.
  }

  TDelimitedProtoStream = class(TObject)
  private
    FFilePath: AnsiString;
    FIsWriter: Boolean;

    // Writer fields
    FFileStream: TFileStream;
    FBuffer: TMemoryStream; // Custom RAM buffer for high-performance, safe writing
    FMaxBufferSize: Integer;

    // Reader fields
    FReadStream: TReadBufStream;

    procedure EnsureWriterIsValid;
    procedure EnsureReaderIsValid;
    procedure FlushBufferToDisk;

  public
    constructor CreateWriter(const AFilePath: AnsiString; ABufferSize: Integer = 65536);
    constructor CreateReader(const AFilePath: AnsiString; ABufferSize: Integer = 131072);
    destructor Destroy; override;

    procedure WriteMessage(AMessage: TBaseMessage);
    function ReadMessage(AMessage: TBaseMessage): Boolean;

    // Forces any remaining data in the buffer to disk
    procedure Flush;
  end;

  { TDelimitedProtoStream }
  TDelimitedProtoStreamList = specialize TList<TDelimitedProtoStream>;
  TAnsiStringList = specialize TList<ansistring>;

  { TDelimitedProtoReader - Generic reader }

  generic TDelimitedProtoReader<T: TBaseMessage> = class(TObject)
  protected
  type
    { TDelimitedProtoShardReader - Manages reading from one shard (multiple parts) }
    TDelimitedProtoShardReader = class(TObject)
    private
      FPartPaths: TStringList;        // All part paths for this shard (sorted)
      FCurrentPartIndex: integer;     // Which part we're currently reading
      FCurrentStream: TDelimitedProtoStream;     // Currently open part stream
      FBufferSize: integer;

      procedure OpenNextPart;
      procedure CloseCurrentPart;

    public
      constructor Create(const AShardDir: ansistring; ABufferSize: integer);
      destructor Destroy; override;

      function ReadMessage(AMessage: TBaseMessage): boolean;
      function HasMoreParts: boolean;
    end;

    TDelimitedProtoShardReaderList = specialize TObjectList<TDelimitedProtoShardReader>;

  private
    FShardReaders: TDelimitedProtoShardReaderList;
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

  { TDelimitedProtoWriter - Generic writer }

  generic TDelimitedProtoWriter<T: TBaseMessage> = class(TObject)
  public
  type
    { Forward declaration }
    TDelimitedProtoShardWriter = class;

    { TDelimitedProtoPartWriter - Writes to a single part file }
    TDelimitedProtoPartWriter = class(TObject)
    private
      FDelimitedProtoStream: TDelimitedProtoStream;
      FPartPath: ansistring;
    public
      constructor Create(const APartPath: ansistring; ABufferSize: integer = 65536);
      destructor Destroy; override;

      procedure WriteMessage(AMessage: T);

      property PartPath: ansistring read FPartPath;
    end;

    { TDelimitedProtoShardWriter - Manages writing to one specific shard }
    TDelimitedProtoShardWriter = class(TObject)
    private
      FParentWriter: TDelimitedProtoWriter;  // Back reference (not owned)
      FShardIndex: integer;
      FSequenceCounter: integer;
      FCurrentPartWriter: TDelimitedProtoPartWriter;  // Reused for WriteMessage
      FBufferSize: integer;

      function GeneratePartPath: ansistring;
      procedure EnsureShardDirectoryExists;

    public
      constructor Create(AParentWriter: TDelimitedProtoWriter; AShardIndex: integer;
        ABufferSize: integer);
      destructor Destroy; override;

      { Creates a new part writer - caller owns and must free }
      function NewPartWriter: TDelimitedProtoPartWriter;

      { Convenience: writes to current part (creates if needed, reuses) }
      procedure WriteMessage(AMessage: T);

      property ShardIndex: integer read FShardIndex;
    end;

    TDelimitedProtoShardWriterList = specialize TObjectList<TDelimitedProtoShardWriter>;
    TDelimitedProtoPartWriterList = specialize TObjectList<TDelimitedProtoPartWriter>;

  private
    FPattern: TPattern;
    FBufferSize: integer;
    FCurrentShardIndex: integer;  // For round-robin in NewPartWriter
    FShardWriters: TDelimitedProtoShardWriterList;  // Lazily created shard writers

    function GetShardDirectoryPath(ShardIndex: integer): ansistring;
    function GetTotalStreams: integer;

  public
    property NumShards: integer read GetTotalStreams;
    constructor Create(APattern: TPattern; ABufferSize: integer = 65536);
    destructor Destroy; override;

    { Get shard writer for specific shard (lazily created) }
    function GetShardWriter(ShardIndex: integer): TDelimitedProtoShardWriter; inline;

    { Convenience: write message to specific shard }
    procedure WriteMessageToShard(AMessage: T; ShardIndex: integer); inline;

    { Create a new part writer with round-robin shard distribution }
    function NewPartWriter: TDelimitedProtoPartWriter;

  end;

function IsFileHandleValid(AStream: THandleStream): Boolean; inline;

implementation

function IsFileHandleValid(AStream: THandleStream): Boolean;
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

{ TDelimitedProtoStream }

constructor TDelimitedProtoStream.CreateWriter(const AFilePath: AnsiString; ABufferSize: Integer);
begin
  inherited Create;
  FIsWriter := True;
  FFilePath := AFilePath;
  FMaxBufferSize := ABufferSize;

  // Pre-allocate buffer with extra space to avoid dynamic resizing during writes
  // Buffer grows naturally as messages are written, then flushes when threshold reached
  FBuffer := TMemoryStream.Create;
  FBuffer.SetSize(FMaxBufferSize + 1024); // +1KB safety margin for VarInt overhead
  FBuffer.Clear;

  // IMPORTANT: File handle is NOT created here!
  // We defer file creation until the first flush. This allows:
  //   1. Fast constructor (no I/O)
  //   2. Atomic file creation (file appears only when first data is written)
  //   3. Ability to recover from handle invalidation
  FFileStream := nil;
end;

constructor TDelimitedProtoStream.CreateReader(const AFilePath: AnsiString; ABufferSize: Integer);
begin
  inherited Create;
  FIsWriter := False;
  FFilePath := AFilePath;
  FMaxBufferSize := ABufferSize;

  // Open file immediately for reading (unlike writer which is lazy)
  // TReadBufStream wraps TFileStream and provides buffered reading to minimize syscalls
  // Typical read pattern: read VarInt (1-5 bytes), then read message (variable size)
  FFileStream := TFileStream.Create(FFilePath, fmOpenRead or fmShareDenyWrite);
  FReadStream := TReadBufStream.Create(FFileStream, FMaxBufferSize);
end;

destructor TDelimitedProtoStream.Destroy;
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

procedure TDelimitedProtoStream.EnsureWriterIsValid;
var
  Mode: Word;
begin
  // Fast path: if handle is valid, do nothing
  if (FFileStream <> nil) and IsFileHandleValid(FFileStream) then
    Exit;

  // Handle recovery: macOS can invalidate file descriptors on sleep/wake
  // We detect this with fpfcntl() and recreate the stream
  if FFileStream <> nil then
  begin
    FFileStream.Free;
    FFileStream := nil;
  end;

  // Choose appropriate mode:
  //   - If file exists: open in read/write mode and seek to end (append)
  //   - If new file: create with write mode
  if FileExists(FFilePath) then
    Mode := fmOpenReadWrite or fmShareDenyWrite
  else
    Mode := fmCreate or fmShareDenyWrite;

  FFileStream := TFileStream.Create(FFilePath, Mode);

  // For existing files, seek to end to append new data
  if Mode = (fmOpenReadWrite or fmShareDenyWrite) then
    FFileStream.Position := FFileStream.Size;
end;

procedure TDelimitedProtoStream.EnsureReaderIsValid;
var
  LogicalPosition: Int64;
begin
  // Fast path: handle is still valid
  if (FFileStream <> nil) and IsFileHandleValid(FFileStream) then
    Exit;

  // Handle recovery for readers:
  // macOS can kill file handles during system sleep. When this happens:
  //   1. Save the current logical read position
  //   2. Close both buffered and raw streams
  //   3. Reopen the file
  //   4. Seek back to the saved position
  // This allows reading to continue transparently after handle invalidation
  if FFileStream <> nil then
  begin
    LogicalPosition := FReadStream.Position;  // Save where we are

    FReadStream.Free;
    FFileStream.Free;

    // Recreate streams
    FFileStream := TFileStream.Create(FFilePath, fmOpenRead or fmShareDenyWrite);
    FReadStream := TReadBufStream.Create(FFileStream, FMaxBufferSize);

    // Resume reading from where we left off
    FReadStream.Position := LogicalPosition;
  end;
end;

procedure TDelimitedProtoStream.FlushBufferToDisk;
begin
  // Nothing to flush if not in writer mode or buffer is empty
  if not FIsWriter or (FBuffer.Size = 0) then
    Exit;

  // Ensure file handle is valid (creates file on first flush if needed)
  EnsureWriterIsValid;

  // Write the entire buffer to disk in one system call
  // This minimizes I/O overhead by batching many small messages into one write
  FFileStream.WriteBuffer(FBuffer.Memory^, FBuffer.Size);

  // Reset buffer for next batch (keeps allocated memory, just resets size to 0)
  FBuffer.Clear;
end;

procedure TDelimitedProtoStream.Flush;
begin
  FlushBufferToDisk;
end;

procedure TDelimitedProtoStream.WriteMessage(AMessage: TBaseMessage);
var
  TempStream: TMemoryStream;
  Writer: TProtoStreamWriter;
begin
  if not FIsWriter or (AMessage = nil) then Exit;

  { SERIALIZATION PROCESS:
    Step 1: Serialize message to temporary stream to determine exact byte size
    Step 2: Write VarInt32 size prefix to buffer
    Step 3: Write raw Protobuf bytes to buffer
    Step 4: Flush to disk if buffer threshold reached
    
    Why use TempStream?
      - Protobuf requires knowing message size BEFORE writing the data
      - We can't compute size without actually serializing
      - So we serialize once to TempStream, measure it, then copy to FBuffer
      
    Output format in buffer/file:
      [VarInt32: N][Protobuf bytes: N bytes][VarInt32: M][Protobuf bytes: M bytes]...
  }

  TempStream := TMemoryStream.Create;
  try
    // Serialize the message to get its exact size
    AMessage.SaveToStream(TempStream);

    // Write the size prefix using Base-128 VarInt encoding (1-5 bytes depending on size)
    Writer := TProtoStreamWriter.Create(FBuffer, False);  // False = don't take ownership
    try
      Writer.WriteRawVarint32(UInt32(TempStream.Size));
    finally
      Writer.Free;
    end;

    // Append the raw Protobuf bytes immediately after the size
    FBuffer.WriteBuffer(TempStream.Memory^, TempStream.Size);
  finally
    TempStream.Free;
  end;

  // Automatic flush when buffer reaches threshold
  // This batches multiple messages into fewer disk writes for efficiency
  if FBuffer.Size >= FMaxBufferSize then
    FlushBufferToDisk;
end;

function TDelimitedProtoStream.ReadMessage(AMessage: TBaseMessage): Boolean;
var
  Reader: TProtoStreamReader;
  MsgSize: UInt32;
begin
  { DESERIALIZATION PROCESS:
    Step 1: Ensure file handle is valid (recover if needed)
    Step 2: Check for EOF - return False if no more data
    Step 3: Read VarInt32 size prefix
    Step 4: Validate message size (detect truncated files)
    Step 5: Deserialize message from stream
    
    Returns:
      - True: message successfully read and parsed
      - False: EOF reached OR file corrupted/truncated OR parse error
      
    Note: This never raises exceptions for EOF - it returns False gracefully.
    This allows simple while-loops: while Stream.ReadMessage(msg) do ProcessMessage(msg);
  }

  Result := False;
  if FIsWriter or (AMessage = nil) then Exit;

  // Ensure file handle is alive (may recover from macOS sleep/wake)
  EnsureReaderIsValid;

  // Graceful EOF detection: no more data to read
  if FReadStream.Position >= FReadStream.Size then Exit;

  Reader := TProtoStreamReader.Create(FReadStream, False);  // False = don't take ownership
  try
    // Read the VarInt32 size prefix (tells us how many bytes to read next)
    MsgSize := Reader.ReadVarUInt32;

    // Corruption detection: ensure file has enough bytes for this message
    // If not, the file was likely truncated (incomplete write)
    if FReadStream.Position + MsgSize > FReadStream.Size then Exit;

    // Deserialize the Protobuf message from the stream
    // TBaseMessageCracker allows us to call the protected LoadFromStream method
    Result := TBaseMessageCracker(AMessage).LoadFromStream(Reader, MsgSize);
  finally
    Reader.Free;
  end;
end;

{ TDelimitedProtoReader.TDelimitedProtoShardReader }

constructor TDelimitedProtoReader.TDelimitedProtoShardReader.Create(const AShardDir: ansistring;
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

  // Find all *.dpb files in shard directory
  if FindFirst(AShardDir + PathDelim + '*.dpb', faAnyFile, SearchRec) = 0 then
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

  // Sort alphabetically (part-0000-xxx.dpb, part-0001-xxx.dpb, ...)
  FPartPaths.Sort;

  // Open first part if available
  if FPartPaths.Count > 0 then
    OpenNextPart;
end;

destructor TDelimitedProtoReader.TDelimitedProtoShardReader.Destroy;
begin
  CloseCurrentPart;
  FPartPaths.Free;
  inherited Destroy;
end;

procedure TDelimitedProtoReader.TDelimitedProtoShardReader.OpenNextPart;
var
  PartPath: ansistring;
begin
  CloseCurrentPart;

  if FCurrentPartIndex < FPartPaths.Count then
  begin
    PartPath := FPartPaths[FCurrentPartIndex];
    FCurrentStream := TDelimitedProtoStream.CreateReader(PartPath, FBufferSize);
  end;
end;

procedure TDelimitedProtoReader.TDelimitedProtoShardReader.CloseCurrentPart;
begin
  if FCurrentStream <> nil then
  begin
    FCurrentStream.Free;
    FCurrentStream := nil;
  end;
end;

function TDelimitedProtoReader.TDelimitedProtoShardReader.ReadMessage(AMessage: TBaseMessage): boolean;
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

function TDelimitedProtoReader.TDelimitedProtoShardReader.HasMoreParts: boolean;
begin
  Result := FCurrentPartIndex < FPartPaths.Count;
end;

{ TDelimitedProtoReader }

constructor TDelimitedProtoReader.Create(const APaths: array of ansistring;
  ABufferSize: integer = 131072);
var
  i: integer;
  ShardReader: TDelimitedProtoShardReader;
  Path: ansistring;
begin
  inherited Create;

  FShardReaders := TDelimitedProtoShardReaderList.Create(True); // Owns objects
  FCurrentShardIndex := 0;
  FBufferSize := ABufferSize;
  FPattern := nil;

  // Create shard readers for each path (assuming directories)
  for i := 0 to High(APaths) do
  begin
    Path := APaths[i];

    if not DirectoryExists(Path) then
      raise EStreamException.CreateFmt('Shard directory not found: %s', [Path]);

    ShardReader := TDelimitedProtoShardReader.Create(Path, FBufferSize);
    FShardReaders.Add(ShardReader);
  end;
end;

constructor TDelimitedProtoReader.Create(APattern: TPattern; ABufferSize: integer = 131072);
var
  i: integer;
  ShardReader: TDelimitedProtoShardReader;
  ShardDir: ansistring;
begin
  inherited Create;

  FShardReaders := TDelimitedProtoShardReaderList.Create(True); // Owns objects
  FCurrentShardIndex := 0;
  FPattern := TPattern.Create(APattern);
  FBufferSize := ABufferSize;

  // Create shard readers for each shard
  for i := 0 to APattern.NumShards - 1 do
  begin
    ShardDir := APattern.GetShardPath(i);

    if not DirectoryExists(ShardDir) then
      raise EStreamException.CreateFmt('Shard directory not found: %s', [ShardDir]);

    ShardReader := TDelimitedProtoShardReader.Create(ShardDir, FBufferSize);
    FShardReaders.Add(ShardReader);
  end;
end;

destructor TDelimitedProtoReader.Destroy;
begin
  FPattern.Free;
  FShardReaders.Free; // Automatically frees all shard readers
  inherited Destroy;
end;

function TDelimitedProtoReader.ReadMessage(var AMessage: T): boolean;
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

function TDelimitedProtoReader.ReadMessageFromShard(ShardIndex: integer; var AMessage: T): boolean;
begin
  Result := False;

  // Validate shard index
  if (ShardIndex < 0) or (ShardIndex >= FShardReaders.Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)',
      [ShardIndex, FShardReaders.Count - 1]);

  Result := FShardReaders[ShardIndex].ReadMessage(AMessage);
end;

function TDelimitedProtoReader.GetTotalStreams: integer;
begin
  Result := FShardReaders.Count;
end;

{ TDelimitedProtoWriter.TDelimitedProtoShardWriter }

constructor TDelimitedProtoWriter.TDelimitedProtoShardWriter.Create(AParentWriter: TDelimitedProtoWriter;
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

destructor TDelimitedProtoWriter.TDelimitedProtoShardWriter.Destroy;
begin
  FCurrentPartWriter.Free;

  inherited Destroy;
end;

function TDelimitedProtoWriter.TDelimitedProtoShardWriter.GeneratePartPath: ansistring;
var
  ShardDir: ansistring;
  Timestamp: int64;
begin
  ShardDir := FParentWriter.GetShardDirectoryPath(FShardIndex);

  // Get current Unix timestamp in milliseconds
  Timestamp := DateTimeToUnix(Now, False) * 1000 + MilliSecondOf(Now);

  // Format: part-{seq:04d}-{timestamp}.dpb for alphabetical sorting
  Result := Format('%s%spart-%4.4d-%d.dpb', [ShardDir, PathDelim,
    FSequenceCounter, Timestamp]);

  Inc(FSequenceCounter);
end;

procedure TDelimitedProtoWriter.TDelimitedProtoShardWriter.EnsureShardDirectoryExists;
var
  ShardDir: ansistring;
begin
  ShardDir := FParentWriter.GetShardDirectoryPath(FShardIndex);
  if not DirectoryExists(ShardDir) then
    ForceDirectories(ShardDir);
end;

function TDelimitedProtoWriter.TDelimitedProtoShardWriter.NewPartWriter: TDelimitedProtoPartWriter;
var
  PartPath: ansistring;
begin
  EnsureShardDirectoryExists;
  PartPath := GeneratePartPath;
  Result := TDelimitedProtoPartWriter.Create(PartPath, FBufferSize);
end;

procedure TDelimitedProtoWriter.TDelimitedProtoShardWriter.WriteMessage(AMessage: T);
begin
  if AMessage = nil then
    Exit;

  FCurrentPartWriter.WriteMessage(AMessage);
end;

{ TDelimitedProtoWriter.TDelimitedProtoPartWriter }

constructor TDelimitedProtoWriter.TDelimitedProtoPartWriter.Create(const APartPath: ansistring;
  ABufferSize: integer = 65536);
begin
  inherited Create;

  FPartPath := APartPath;

  FDelimitedProtoStream := TDelimitedProtoStream.CreateWriter(APartPath, ABufferSize);
end;

destructor TDelimitedProtoWriter.TDelimitedProtoPartWriter.Destroy;
begin
  FDelimitedProtoStream.Free;
  inherited Destroy;
end;

procedure TDelimitedProtoWriter.TDelimitedProtoPartWriter.WriteMessage(AMessage: T);
begin
  if AMessage = nil then
    Exit;
  FDelimitedProtoStream.WriteMessage(AMessage);
end;

{ TDelimitedProtoWriter }

constructor TDelimitedProtoWriter.Create(APattern: TPattern; ABufferSize: integer = 65536);
var
  ShardIndex: integer;
begin
  inherited Create;

  FPattern := TPattern.Create(APattern);
  FBufferSize := ABufferSize;
  FCurrentShardIndex := 0;
  FShardWriters := TDelimitedProtoShardWriterList.Create(True);  // Owns shard writers
  FShardWriters.Count := FPattern.NumShards;
  for ShardIndex := 0 to FPattern.NumShards - 1 do
    FShardWriters[ShardIndex] := TDelimitedProtoShardWriter.Create(Self, ShardIndex, FBufferSize);
end;

destructor TDelimitedProtoWriter.Destroy;
begin
  FShardWriters.Free;
  FPattern.Free;
  inherited Destroy;
end;

function TDelimitedProtoWriter.GetShardDirectoryPath(ShardIndex: integer): ansistring;
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

function TDelimitedProtoWriter.GetShardWriter(ShardIndex: integer): TDelimitedProtoShardWriter;
begin
  Result := FShardWriters[ShardIndex];
end;

procedure TDelimitedProtoWriter.WriteMessageToShard(AMessage: T; ShardIndex: integer);
var
  ShardWriter: TDelimitedProtoShardWriter;
begin
  if AMessage = nil then
    Exit;

  ShardWriter := GetShardWriter(ShardIndex);
  ShardWriter.WriteMessage(AMessage);
end;

function TDelimitedProtoWriter.GetTotalStreams: integer;
begin
  Result := FPattern.NumShards;
end;

function TDelimitedProtoWriter.NewPartWriter: TDelimitedProtoPartWriter;
var
  ShardWriter: TDelimitedProtoShardWriter;
begin
  { Round-robin distribution across shards }
  ShardWriter := GetShardWriter(FCurrentShardIndex);
  Result := ShardWriter.NewPartWriter;
  
  { Move to next shard for next call }
  FCurrentShardIndex := (FCurrentShardIndex + 1) mod FPattern.NumShards;
end;

end.
