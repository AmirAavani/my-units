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
    constructor Create(const Pattern: TPattern);
    constructor Create(const Pattern: ansistring);
    constructor Create(const ABasePath: ansistring; ANumShards: integer);
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
    FStream: TStream;          // The buffered stream (for I/O)
    FFileStream: TFileStream;  // The underlying file stream (may be nil)
    FOwnsStreams: boolean;

  public
    constructor Create(AStream: TStream; AOwnsStreams: boolean = False); overload;
    constructor Create(AFileStream: TFileStream; ABufferedStream: TStream;
      AOwnsStreams: boolean = False); overload;
    destructor Destroy; override;

    procedure WriteMessage(AMessage: TBaseMessage);
    function ReadMessage(AMessage: TBaseMessage): boolean;

    property Stream: TStream read FStream;
  end;

  TZioStreamList = specialize TList<TZioStream>;
  TAnsiStringList = specialize TList<ansistring>;

  { TZioStreams }

  TZioStreams = class(TZioStreamList)
  private
    FPath: ansistring;
    FNumStreams: integer;

  public
    constructor Create(const APath: ansistring; ANumStreams: integer); overload;
    constructor Create(APattern: TPattern); overload;
    destructor Destroy; override;

    procedure WriteMessageToShard(AMessage: TBaseMessage; ShardIndex: integer);
  end;

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

    TZioPartWriterList = class(specialize TObjectList<TZioPartWriter>);

    { TZioShardWriterList }

    TZioShardWriterList = class(specialize TObjectList<TZioShardWriter>)
    public
      { Creates a new part writer - caller owns and must free }
      function NewPartWriters: TZioPartWriterList;

    end;


  private
    FPattern: TPattern;
    FBufferSize: integer;
    FCurrentShardIndex: integer;  // For round-robin in NewPartWriter
    FShardWriters: TZioShardWriterList;  // Lazily created shard writers

    function GetOrCreateShardWriter(ShardIndex: integer): TZioShardWriter;
    function GetShardDirectoryPath(ShardIndex: integer): ansistring;
    function GetTotalStreams: integer;

  public
    property NumShards: integer read GetTotalStreams;
    constructor Create(APattern: TPattern; ABufferSize: integer = 65536);
    destructor Destroy; override;

    { Get shard writer for specific shard (lazily created) }
    function GetShardWriter(ShardIndex: integer): TZioShardWriter;

    { Convenience: write message to specific shard }
    procedure WriteMessageToShard(AMessage: T; ShardIndex: integer);

    { Creates part writer with round-robin shard selection }
    function NewPartWriter: TZioPartWriter;

    { Creates list of part writers with one TZioPartWriter per shard }
    function NewPartWriters: TZioPartWriterList;
  end;


implementation

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

constructor TPattern.Create(const Pattern: TPattern);
begin
  inherited Create;

  FBasePath := Pattern.FBasePath;
  FNumShards := Pattern.FNumShards;
  FRemainder := Pattern.FRemainder;
  FModulo := Pattern.FModulo;

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

constructor TZioStream.Create(AStream: TStream; AOwnsStreams: boolean);
begin
  inherited Create;
  FStream := AStream;
  FFileStream := nil;
  FOwnsStreams := AOwnsStreams;
end;

constructor TZioStream.Create(AFileStream: TFileStream; ABufferedStream: TStream;
  AOwnsStreams: boolean);
begin
  inherited Create;
  FStream := ABufferedStream;
  FFileStream := AFileStream;
  FOwnsStreams := AOwnsStreams;
end;

destructor TZioStream.Destroy;
begin
  if FOwnsStreams then
  begin
    // Free buffered stream first (this flushes buffers to underlying stream)
    if FStream <> nil then
      FStream.Free;
    // Then free underlying file stream (this closes the file)
    if FFileStream <> nil then
      FFileStream.Free;
  end;

  inherited Destroy;
end;

procedure TZioStream.WriteMessage(AMessage: TBaseMessage);
var
  TempStream: TMemoryStream;
  Writer: TProtoStreamWriter;
  Header: array[0..11] of ansichar;
begin
  if AMessage = nil then
    Exit;

  FillChar(Header, SizeOf(Header), #32);
  Move(pansichar('ZIO1PBUF')^, Header[0], 8);

  TempStream := TMemoryStream.Create;
  AMessage.SaveToStream(TempStream);

  FStream.WriteBuffer(Header[0], 12);

  Writer := TProtoStreamWriter.Create(FStream, False);
  Writer.WriteRawVarint32(TempStream.Size);
  Writer.Free;

  FStream.WriteBuffer(TempStream.Memory^, TempStream.Size);

  TempStream.Free;
end;

function TZioStream.ReadMessage(AMessage: TBaseMessage): boolean;
var
  Reader: TProtoStreamReader;
  Header: array[0..11] of ansichar;
  MsgSize: uint32;
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

{ TZioStreams }

constructor TZioStreams.Create(const APath: ansistring; ANumStreams: integer);
var
  i: integer;
  ShardPath: ansistring;
  FileStream: TFileStream;
  BufferedStream: TWriteBufStream;
  ZioStream: TZioStream;
  Pattern: TPattern;
begin
  inherited Create;

  FPath := APath;
  FNumStreams := ANumStreams;

  // Ensure directory exists
  ForceDirectories(FPath);

  // Use TPattern to generate shard paths
  Pattern := TPattern.Create(FPath, FNumStreams);
  try
    for i := 0 to ANumStreams - 1 do
    begin
      ShardPath := Pattern.GetShardPath(i);

      FileStream := TFileStream.Create(ShardPath, fmCreate);
      BufferedStream := TWriteBufStream.Create(FileStream, 65536);  // 64KB write buffer
      ZioStream := TZioStream.Create(FileStream, BufferedStream, True);
      Add(ZioStream);
    end;
  finally
    Pattern.Free;
  end;
end;

constructor TZioStreams.Create(APattern: TPattern);
var
  i: integer;
  ShardPath: ansistring;
  FileStream: TFileStream;
  BufferedStream: TWriteBufStream;
  ZioStream: TZioStream;
begin
  inherited Create;

  FPath := APattern.BasePath;
  FNumStreams := APattern.NumShards;

  // Ensure directory exists
  ForceDirectories(FPath);

  // Use pattern to generate shard paths
  for i := 0 to APattern.NumShards - 1 do
  begin
    ShardPath := APattern.GetShardPath(i);

    FileStream := TFileStream.Create(ShardPath, fmCreate);
    BufferedStream := TWriteBufStream.Create(FileStream, 65536);  // 64KB write buffer
    ZioStream := TZioStream.Create(FileStream, BufferedStream, True);
    Add(ZioStream);
  end;
end;

destructor TZioStreams.Destroy;
var
  ZioStream: TZioStream;
begin
  // Free all TZioStream objects (they will free their underlying streams)
  for ZioStream in Self do
    ZioStream.Free;

  inherited Destroy;
end;

procedure TZioStreams.WriteMessageToShard(AMessage: TBaseMessage; ShardIndex: integer);
begin
  if (ShardIndex < 0) or (ShardIndex >= Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)',
      [ShardIndex, Count - 1]);

  Items[ShardIndex].WriteMessage(AMessage);
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
  FileStream: TFileStream;
  BufferedStream: TReadBufStream;
  PartPath: ansistring;
begin
  CloseCurrentPart;

  if FCurrentPartIndex < FPartPaths.Count then
  begin
    PartPath := FPartPaths[FCurrentPartIndex];

    try
      FileStream := TFileStream.Create(PartPath, fmOpenRead);
      BufferedStream := TReadBufStream.Create(FileStream, FBufferSize);
      FCurrentStream := TZioStream.Create(FileStream, BufferedStream, True);
    except
      on E: Exception do
        raise EStreamException.CreateFmt('Failed to open part file "%s": %s',
          [PartPath, E.Message]);
    end;
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
  FPattern := APattern;
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
  FCurrentPartWriter := nil;
  FBufferSize := ABufferSize;
end;

destructor TZioWriter.TZioShardWriter.Destroy;
begin
  // Free the current part writer if it exists
  if FCurrentPartWriter <> nil then
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

  // Create part writer if needed (reuse for subsequent calls)
  if FCurrentPartWriter = nil then
  begin
    EnsureShardDirectoryExists;
    FCurrentPartWriter := NewPartWriter;
  end;

  FCurrentPartWriter.WriteMessage(AMessage);
end;

{ TZioWriter.TZioShardWriterList }

function TZioWriter.TZioShardWriterList.NewPartWriters: TZioPartWriterList;
var
  Writer: TZioShardWriter;
begin
  Result := TZioPartWriterList.Create;
  Result.Capacity := Self.Count;

  for Writer in Self do
    Result.Add(Writer.NewPartWriter);
end;

{ TZioWriter.TZioPartWriter }

constructor TZioWriter.TZioPartWriter.Create(const APartPath: ansistring;
  ABufferSize: integer = 65536);
var
  FileStream: TFileStream;
  BufferedStream: TWriteBufStream;
begin
  inherited Create;

  FPartPath := APartPath;

  try
    FileStream := TFileStream.Create(APartPath, fmCreate);
    BufferedStream := TWriteBufStream.Create(FileStream, ABufferSize);
    FZioStream := TZioStream.Create(FileStream, BufferedStream, True);
  except
    on E: Exception do
      raise EStreamException.CreateFmt('Failed to create part file "%s": %s',
        [APartPath, E.Message]);
  end;
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
begin
  inherited Create;

  FPattern := TPattern.Create(APattern);
  FBufferSize := ABufferSize;
  FCurrentShardIndex := 0;
  FShardWriters := TZioShardWriterList.Create(True);  // Owns shard writers
  FShardWriters.Count := FPattern.NumShards;

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

function TZioWriter.GetOrCreateShardWriter(ShardIndex: integer): TZioShardWriter;
begin
  // Validate shard index
  if (ShardIndex < 0) or (ShardIndex >= FPattern.NumShards) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)',
      [ShardIndex, FPattern.NumShards - 1]);

  // Expand list if needed
  while FShardWriters.Count <= ShardIndex do
    FShardWriters.Add(nil);

  // Create shard writer if it doesn't exist
  if FShardWriters[ShardIndex] = nil then
    FShardWriters[ShardIndex] := TZioShardWriter.Create(Self, ShardIndex, FBufferSize);

  Result := FShardWriters[ShardIndex];
end;

function TZioWriter.GetShardWriter(ShardIndex: integer): TZioShardWriter;
begin
  Result := GetOrCreateShardWriter(ShardIndex);
end;

procedure TZioWriter.WriteMessageToShard(AMessage: T; ShardIndex: integer);
var
  ShardWriter: TZioShardWriter;
begin
  if AMessage = nil then
    Exit;

  ShardWriter := GetOrCreateShardWriter(ShardIndex);
  ShardWriter.WriteMessage(AMessage);
end;

function TZioWriter.NewPartWriter: TZioPartWriter;
var
  ShardIndex: integer;
  ShardWriter: TZioShardWriter;
begin
  // Get current shard in round-robin fashion
  ShardIndex := FCurrentShardIndex;
  FCurrentShardIndex := (FCurrentShardIndex + 1) mod FPattern.NumShards;

  // Get or create shard writer and create new part
  ShardWriter := GetOrCreateShardWriter(ShardIndex);
  Result := ShardWriter.NewPartWriter;
end;

function TZioWriter.NewPartWriters: TZioPartWriterList;
var
  ShardIndex: integer;
begin
  Result := TZioPartWriterList.Create;
  Result.Capacity := Self.NumShards;

  for ShardIndex := 0 to FPattern.NumShards - 1 do
    Result.Add(GetOrCreateShardWriter(ShardIndex).NewPartWriter);

end;

function TZioWriter.GetTotalStreams: integer;
begin
  Result := FPattern.NumShards;
end;

end.
