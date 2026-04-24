unit ZIOStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoStreamUnit, ProtoHelperUnit, Generics.Collections, SyncObjs;

type
  TAnsiStringArray = array of AnsiString;

  { Exception Classes }

  EZIOStreamException = class(Exception);
  
  EPatternException = class(EZIOStreamException);
  
  EShardException = class(EZIOStreamException);
  
  EStreamException = class(EZIOStreamException);

  { TPattern - Encapsulates shard path pattern }
  
  TPattern = class(TObject)
  private
    FBasePath: AnsiString;
    FNumShards: Integer;
    
  public
    constructor Create(const Pattern: AnsiString);
    constructor Create(const ABasePath: AnsiString; ANumShards: Integer);
    
    function GetShardPath(ShardIndex: Integer): AnsiString;
    function GetAllPaths: TAnsiStringArray;
    
    property BasePath: AnsiString read FBasePath;
    property NumShards: Integer read FNumShards;
  end;

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

  TZioStreamList = specialize TList<TZioStream>;
  TAnsiStringList = specialize TList<AnsiString>;
  TCriticalSectionList = specialize TList<TCriticalSection>;

  { TZioStreams }

  TZioStreams = class(TZioStreamList)
  private
    FPath: AnsiString;
    FNumStreams: Integer;

  public
    constructor Create(const APath: AnsiString; ANumStreams: Integer); overload;
    constructor Create(APattern: TPattern); overload;
    destructor Destroy; override;

    procedure WriteMessageToShard(AMessage: TBaseMessage; ShardIndex: Integer);
  end;

  { TZioReader - Generic thread-safe ZIO reader }

  generic TZioReader<T: TBaseMessage> = class(TObject)
  private
    FStreams: TZioStreamList;
    FPaths: TAnsiStringList;
    FShardMutexes: TCriticalSectionList;
    FCurrentStreamIndex: Integer;
    FGlobalMutex: TCriticalSection;  // For protecting FCurrentStreamIndex
    FMessage: T;

  protected
    function GetTotalStreams: Integer;

  public
    property NumShards: Integer read GetTotalStreams;
    constructor Create(const APaths: array of AnsiString); overload;
    constructor Create(APattern: TPattern); overload;
    destructor Destroy; override;

    function ReadMessage(out AMessage: T): Boolean;
    function ReadMessageFromShard(ShardIndex: Integer; out AMessage: T): Boolean;
  end;

  { TZioWriter - Generic thread-safe ZIO writer }

  generic TZioWriter<T: TBaseMessage> = class(TObject)
  private
    FStreams: TZioStreamList;
    FPaths: TAnsiStringList;
    FShardMutexes: TCriticalSectionList;
    FCurrentShardIndex: Integer;
    FGlobalMutex: TCriticalSection;  // For protecting FCurrentShardIndex

  protected
    function GetTotalStreams: Integer;

  public
    property NumShards: Integer read GetTotalStreams;
    constructor Create(const APaths: array of AnsiString); overload;
    constructor Create(APattern: TPattern); overload;
    destructor Destroy; override;

    procedure WriteMessage(AMessage: T);
    procedure WriteMessageToShard(AMessage: T; ShardIndex: Integer);
  end;


implementation

type
  { Cracker class to access protected LoadFromStream }
  TBaseMessageCracker = class(TBaseMessage);

{ TPattern }

constructor TPattern.Create(const Pattern: AnsiString);
var
  AtPos: Integer;
  NumShardsStr: AnsiString;
begin
  inherited Create;
  
  // Parse "path@N" format
  AtPos := Pos('@', Pattern);
  if AtPos = 0 then
    raise EPatternException.CreateFmt('Invalid pattern format: "%s". Expected "path@numShards"', [Pattern]);
  
  FBasePath := Copy(Pattern, 1, AtPos - 1);
  NumShardsStr := Copy(Pattern, AtPos + 1, Length(Pattern));
  
  if not TryStrToInt(NumShardsStr, FNumShards) then
    raise EPatternException.CreateFmt('Invalid number of shards: "%s"', [NumShardsStr]);
  
  if FNumShards <= 0 then
    raise EPatternException.CreateFmt('Number of shards must be positive: %d', [FNumShards]);
end;

constructor TPattern.Create(const ABasePath: AnsiString; ANumShards: Integer);
begin
  inherited Create;
  
  FBasePath := ABasePath;
  FNumShards := ANumShards;
  
  if FNumShards <= 0 then
    raise EPatternException.CreateFmt('Number of shards must be positive: %d', [FNumShards]);
end;

function TPattern.GetShardPath(ShardIndex: Integer): AnsiString;
begin
  if (ShardIndex < 0) or (ShardIndex >= FNumShards) then
    raise EShardException.CreateFmt('Shard index %d out of range [0..%d]', [ShardIndex, FNumShards - 1]);
  
  Result := Format('%sshards-%4.4d-%4.4d.zio',
                   [IncludeTrailingPathDelimiter(FBasePath), ShardIndex, FNumShards]);
end;

function TPattern.GetAllPaths: TAnsiStringArray;
var
  i: Integer;
begin
  SetLength(Result, FNumShards);
  for i := 0 to FNumShards - 1 do
    Result[i] := GetShardPath(i);
end;

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

{ TZioStreams }

constructor TZioStreams.Create(const APath: AnsiString; ANumStreams: Integer);
var
  i: Integer;
  ShardPath: AnsiString;
  FileStream: TFileStream;
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
      ZioStream := TZioStream.Create(FileStream, True);
      Add(ZioStream);
    end;
  finally
    Pattern.Free;
  end;
end;

constructor TZioStreams.Create(APattern: TPattern);
var
  i: Integer;
  ShardPath: AnsiString;
  FileStream: TFileStream;
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
    ZioStream := TZioStream.Create(FileStream, True);
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

procedure TZioStreams.WriteMessageToShard(AMessage: TBaseMessage; ShardIndex: Integer);
begin
  if (ShardIndex < 0) or (ShardIndex >= Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)', [ShardIndex, Count - 1]);
  
  Items[ShardIndex].WriteMessage(AMessage);
end;

{ TZioReader }

constructor TZioReader.Create(const APaths: array of AnsiString);
var
  i: Integer;
  FileStream: TFileStream;
  ZioStream: TZioStream;
  Path: AnsiString;
begin
  inherited Create;
  
  FStreams := TZioStreamList.Create;
  FPaths := TAnsiStringList.Create;
  FShardMutexes := TCriticalSectionList.Create;
  FGlobalMutex := TCriticalSection.Create;
  FCurrentStreamIndex := 0;
  FMessage := T.Create;
  
  // Store paths and open all files
  for i := 0 to High(APaths) do
  begin
    Path := APaths[i];
    FPaths.Add(Path);
    
    if not FileExists(Path) then
      raise EStreamException.CreateFmt('File not found: %s', [Path]);
    
    try
      FileStream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    except
      on E: Exception do
        raise EStreamException.CreateFmt('Failed to open file "%s": %s', [Path, E.Message]);
    end;
    
    ZioStream := TZioStream.Create(FileStream, True);
    FStreams.Add(ZioStream);
    
    // Create a mutex for this shard
    FShardMutexes.Add(TCriticalSection.Create);
  end;
end;

constructor TZioReader.Create(APattern: TPattern);
var
  i: Integer;
  FileStream: TFileStream;
  ZioStream: TZioStream;
  Path: AnsiString;
begin
  inherited Create;
  
  FStreams := TZioStreamList.Create;
  FPaths := TAnsiStringList.Create;
  FShardMutexes := TCriticalSectionList.Create;
  FGlobalMutex := TCriticalSection.Create;
  FCurrentStreamIndex := 0;
  FMessage := T.Create;
  
  // Use pattern to open all shard files
  for i := 0 to APattern.NumShards - 1 do
  begin
    Path := APattern.GetShardPath(i);
    FPaths.Add(Path);
    
    if not FileExists(Path) then
      raise EStreamException.CreateFmt('File not found: %s', [Path]);
    
    try
      FileStream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    except
      on E: Exception do
        raise EStreamException.CreateFmt('Failed to open file "%s": %s', [Path, E.Message]);
    end;
    
    ZioStream := TZioStream.Create(FileStream, True);
    FStreams.Add(ZioStream);
    
    // Create a mutex for this shard
    FShardMutexes.Add(TCriticalSection.Create);
  end;
end;

destructor TZioReader.Destroy;
var
  ZioStream: TZioStream;
  Mutex: TCriticalSection;
begin
  // Free all TZioStream objects (they will free their underlying streams)
  for ZioStream in FStreams do
    ZioStream.Free;
  
  // Free all shard mutexes
  for Mutex in FShardMutexes do
    Mutex.Free;
  
  FStreams.Free;
  FPaths.Free;
  FShardMutexes.Free;
  FGlobalMutex.Free;
  FMessage.Free;
  
  inherited Destroy;
end;

function TZioReader.ReadMessage(out AMessage: T): Boolean;
var
  LocalStreamIndex: Integer;
begin
  Result := False;
  AMessage := nil;
  
  // Use global mutex to get next stream index safely
  FGlobalMutex.Enter;
  try
    if FCurrentStreamIndex >= FStreams.Count then
      Exit; // All streams exhausted
    
    LocalStreamIndex := FCurrentStreamIndex;
  finally
    FGlobalMutex.Leave;
  end;
  
  // Try to read from streams sequentially until we find a message or exhaust all streams
  while LocalStreamIndex < FStreams.Count do
  begin
    // Lock only the specific shard we're reading from
    FShardMutexes[LocalStreamIndex].Enter;
    try
      Result := FStreams[LocalStreamIndex].ReadMessage(FMessage);
      
      if Result then
      begin
        AMessage := FMessage;
        Exit; // Successfully read a message
      end;
    finally
      FShardMutexes[LocalStreamIndex].Leave;
    end;
    
    // Current stream is exhausted, move to next
    FGlobalMutex.Enter;
    try
      // Re-check in case another thread advanced it
      if FCurrentStreamIndex = LocalStreamIndex then
        Inc(FCurrentStreamIndex);
      
      if FCurrentStreamIndex >= FStreams.Count then
        Exit; // All streams exhausted
      
      LocalStreamIndex := FCurrentStreamIndex;
    finally
      FGlobalMutex.Leave;
    end;
  end;
  
  // All streams exhausted
  Result := False;
end;

function TZioReader.ReadMessageFromShard(ShardIndex: Integer; out AMessage: T): Boolean;
begin
  Result := False;
  AMessage := nil;
  
  // Validate shard index (no lock needed for validation)
  if (ShardIndex < 0) or (ShardIndex >= FStreams.Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)', 
                              [ShardIndex, FStreams.Count - 1]);
  
  // Lock only the specific shard we're reading from
  FShardMutexes[ShardIndex].Enter;
  try
    Result := FStreams[ShardIndex].ReadMessage(FMessage);
    
    if Result then
      AMessage := FMessage;
  finally
    FShardMutexes[ShardIndex].Leave;
  end;
end;

function TZioReader.GetTotalStreams: Integer;
begin
  Result := FStreams.Count;
end;

{ TZioWriter }

constructor TZioWriter.Create(const APaths: array of AnsiString);
var
  i: Integer;
  FileStream: TFileStream;
  ZioStream: TZioStream;
  Path: AnsiString;
  DirPath: AnsiString;
begin
  inherited Create;
  
  FStreams := TZioStreamList.Create;
  FPaths := TAnsiStringList.Create;
  FShardMutexes := TCriticalSectionList.Create;
  FGlobalMutex := TCriticalSection.Create;
  FCurrentShardIndex := 0;
  
  // Create all shard files
  for i := 0 to High(APaths) do
  begin
    Path := APaths[i];
    FPaths.Add(Path);
    
    // Ensure directory exists
    DirPath := ExtractFileDir(Path);
    if DirPath <> '' then
      ForceDirectories(DirPath);
    
    try
      FileStream := TFileStream.Create(Path, fmCreate);
    except
      on E: Exception do
        raise EStreamException.CreateFmt('Failed to create file "%s": %s', [Path, E.Message]);
    end;
    
    ZioStream := TZioStream.Create(FileStream, True);
    FStreams.Add(ZioStream);
    
    // Create a mutex for this shard
    FShardMutexes.Add(TCriticalSection.Create);
  end;
end;

constructor TZioWriter.Create(APattern: TPattern);
var
  i: Integer;
  FileStream: TFileStream;
  ZioStream: TZioStream;
  Path: AnsiString;
begin
  inherited Create;
  
  FStreams := TZioStreamList.Create;
  FPaths := TAnsiStringList.Create;
  FShardMutexes := TCriticalSectionList.Create;
  FGlobalMutex := TCriticalSection.Create;
  FCurrentShardIndex := 0;
  
  // Ensure directory exists
  ForceDirectories(APattern.BasePath);
  
  // Use pattern to create all shard files
  for i := 0 to APattern.NumShards - 1 do
  begin
    Path := APattern.GetShardPath(i);
    FPaths.Add(Path);
    
    try
      FileStream := TFileStream.Create(Path, fmCreate);
    except
      on E: Exception do
        raise EStreamException.CreateFmt('Failed to create file "%s": %s', [Path, E.Message]);
    end;
    
    ZioStream := TZioStream.Create(FileStream, True);
    FStreams.Add(ZioStream);
    
    // Create a mutex for this shard
    FShardMutexes.Add(TCriticalSection.Create);
  end;
end;

destructor TZioWriter.Destroy;
var
  ZioStream: TZioStream;
  Mutex: TCriticalSection;
begin
  // Free all TZioStream objects (they will free their underlying streams)
  for ZioStream in FStreams do
    ZioStream.Free;
  
  // Free all shard mutexes
  for Mutex in FShardMutexes do
    Mutex.Free;
  
  FStreams.Free;
  FPaths.Free;
  FShardMutexes.Free;
  FGlobalMutex.Free;
  
  inherited Destroy;
end;

procedure TZioWriter.WriteMessage(AMessage: T);
var
  ShardIndex: Integer;
begin
  if AMessage = nil then
    Exit;
  
  // Get current shard in round-robin fashion
  FGlobalMutex.Enter;
  try
    ShardIndex := FCurrentShardIndex;
    FCurrentShardIndex := (FCurrentShardIndex + 1) mod FStreams.Count;
  finally
    FGlobalMutex.Leave;
  end;
  
  // Write to the selected shard (protected by shard-specific mutex)
  FShardMutexes[ShardIndex].Enter;
  try
    FStreams[ShardIndex].WriteMessage(AMessage);
  finally
    FShardMutexes[ShardIndex].Leave;
  end;
end;

procedure TZioWriter.WriteMessageToShard(AMessage: T; ShardIndex: Integer);
begin
  if AMessage = nil then
    Exit;
  
  // Validate shard index
  if (ShardIndex < 0) or (ShardIndex >= FStreams.Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)', 
                              [ShardIndex, FStreams.Count - 1]);
  
  // Write to the specific shard (protected by shard-specific mutex)
  FShardMutexes[ShardIndex].Enter;
  try
    FStreams[ShardIndex].WriteMessage(AMessage);
  finally
    FShardMutexes[ShardIndex].Leave;
  end;
end;

function TZioWriter.GetTotalStreams: Integer;
begin
  Result := FStreams.Count;
end;

end.
