unit ZIOStreamUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bufstream, syncobjs, ProtoStreamUnit, ProtoHelperUnit,
  Generics.Collections, DateUtils;

const
  DEFAULT_MAX_PART_SIZE = 1073741824; // 1 GiB soft limit

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
    function WithModulo(Remainder, ModuloValue: integer): TPattern;

    property BasePath: ansistring read FBasePath;
    property NumShards: integer read GetFilteredNumShards;
  end;

  { TRobustFileStream - Retries partial writes under kernel backpressure }

  TRobustFileStream = class(TFileStream)
  public
    function Write(const Buffer; Count: longint): longint; override;
  end;

  { TZioStream }

  TZioStream = class(TObject)
  private
    FBufferedStream: TBufStream;
    FFileStream: TFileStream;
    FOwnsStreams: boolean;
    FBytesWritten: int64;
    FTempStream: TMemoryStream;

  public
    constructor Create(ABufferedStream: TBufStream; AOwnsStreams: boolean = False);
      overload;
    constructor Create(AFileStream: TFileStream; ABufferedStream: TBufStream;
      AOwnsStreams: boolean = False); overload;
    destructor Destroy; override;

    procedure WriteMessage(AMessage: TBaseMessage);
    function ReadMessage(AMessage: TBaseMessage): boolean;

    property Stream: TBufStream read FBufferedStream;
    property BytesWritten: int64 read FBytesWritten;
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
    TZioShardReader = class(TObject)
    private
      FPartPaths: TStringList;
      FCurrentPartIndex: integer;
      FCurrentStream: TZioStream;
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
    TZioShardWriter = class;

    { TZioPartWriter - Handles automatic stream rotation at MaxPartSize }
    TZioPartWriter = class(TObject)
    private
      FShardWriter: TZioShardWriter;
      FZioStream: TZioStream;
      FPartPath: ansistring;
      FBufferSize: integer;
      FMaxPartSize: int64;

      procedure OpenNextPart;
      procedure CloseCurrentPart;
      function GetBytesWritten: int64;
    public
      constructor Create(AShardWriter: TZioShardWriter;
        ABufferSize: integer = 65536; AMaxPartSize: int64 = DEFAULT_MAX_PART_SIZE);
        overload;
      constructor Create(const APartPath: ansistring;
        ABufferSize: integer = 65536); overload;
      destructor Destroy; override;

      procedure WriteMessage(AMessage: T);

      property PartPath: ansistring read FPartPath;
      property BytesWritten: int64 read GetBytesWritten;
    end;

    TZioShardWriter = class(TObject)
    private
      FParentWriter: TZioWriter;
      FShardIndex: integer;
      FSequenceCounter: integer;
      FCurrentPartWriter: TZioPartWriter;
      FBufferSize: integer;
      FLock: TCriticalSection; // Mutex for thread-safe path generation

      function GeneratePartPath: ansistring;

    public
      constructor Create(AParentWriter: TZioWriter; AShardIndex: integer;
        ABufferSize: integer);
      destructor Destroy; override;

      function NewPartWriter: TZioPartWriter;
      procedure WriteMessage(AMessage: T);

      property ShardIndex: integer read FShardIndex;
    end;

    { TZioPartWriterList }

    TZioPartWriterList = class(specialize TObjectList<TZioPartWriter>)
    public
      function WriteMessage(Key: UInt32; Message: T): Boolean;

    end;

    TZioShardWriterList = class(specialize TObjectList<TZioShardWriter>)
    public
      function NewPartWriters: TZioPartWriterList;
    end;

  private
    FPattern: TPattern;
    FBufferSize: integer;
    FCurrentShardIndex: integer;
    FShardWriters: TZioShardWriterList;
    FTimestamp: int64;
    FMaxPartSize: int64;
    FLock: TCriticalSection; // Mutex for thread-safe shard writer creation

    function GetOrCreateShardWriter(ShardIndex: integer): TZioShardWriter;
    function GetTotalStreams: integer;

  public
    property NumShards: integer read GetTotalStreams;
    property Timestamp: int64 read FTimestamp;
    property MaxPartSize: int64 read FMaxPartSize;

    constructor Create(APattern: TPattern; ABufferSize: integer = 65536;
      AMaxPartSize: int64 = DEFAULT_MAX_PART_SIZE);
    destructor Destroy; override;

    function GetShardWriter(ShardIndex: integer): TZioShardWriter;
    procedure WriteMessageToShard(AMessage: T; ShardIndex: integer);
    function NewPartWriter: TZioPartWriter;
    function NewPartWriters: TZioPartWriterList;
  end;

implementation
uses
  ALoggerUnit;

type
  TBaseMessageCracker = class(TBaseMessage);

  { TRobustFileStream }

function TRobustFileStream.Write(const Buffer; Count: longint): longint;
var
  TotalWritten, Written: longint;
  Ptr: pbyte;
begin
  TotalWritten := 0;
  Ptr := pbyte(@Buffer);

  while TotalWritten < Count do
  begin
    Written := inherited Write((Ptr + TotalWritten)^, Count - TotalWritten);
    if Written <= 0 then
      Break;
    Inc(TotalWritten, Written);
  end;

  Result := TotalWritten;
end;

{ TPattern }

constructor TPattern.Create(const Pattern: ansistring);
var
  AtPos: integer;
  NumShardsStr: ansistring;
begin
  inherited Create;

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
  FNumShards := Pattern.FNumShards;
  FRemainder := Pattern.FRemainder;
  FModulo := Pattern.FModulo;
end;

destructor TPattern.Destroy;
begin
  inherited Destroy;
end;

function TPattern.GetFilteredNumShards: integer;
begin
  if FModulo = 1 then
    Exit(FNumShards);

  if FRemainder >= FNumShards then
    Exit(0);

  Result := ((FNumShards - 1 - FRemainder) div FModulo) + 1;
end;

function TPattern.GetShardPath(ShardIndex: integer): ansistring;
var
  ActualShardIndex: integer;
begin
  if (ShardIndex < 0) or (ShardIndex >= GetFilteredNumShards) then
    raise EShardException.CreateFmt(
      'Shard index %d out of range [0..%d] for filtered pattern',
      [ShardIndex, GetFilteredNumShards - 1]);

  ActualShardIndex := FRemainder + (ShardIndex * FModulo);

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

constructor TZioStream.Create(ABufferedStream: TBufStream; AOwnsStreams: boolean);
begin
  inherited Create;

  FBufferedStream := ABufferedStream;
  FFileStream := nil;
  FOwnsStreams := AOwnsStreams;
  FBytesWritten := 0;
  FTempStream := TMemoryStream.Create;

end;

constructor TZioStream.Create(AFileStream: TFileStream; ABufferedStream: TBufStream;
  AOwnsStreams: boolean);
begin
  inherited Create;

  FBufferedStream := ABufferedStream;
  FFileStream := AFileStream;
  FOwnsStreams := AOwnsStreams;
  FBytesWritten := 0;
  FTempStream := TMemoryStream.Create;

end;

destructor TZioStream.Destroy;
begin
  if FOwnsStreams then
  begin
    if FBufferedStream <> nil then
      FBufferedStream.Free;
    if FFileStream <> nil then
      FFileStream.Free;
  end;
  FTempStream.Free;

  inherited Destroy;
end;

procedure TZioStream.WriteMessage(AMessage: TBaseMessage);
var
  Header: array[0..11] of ansichar;
  VarintBuf: array[0..4] of byte;
  VarintLen, Value: cardinal;
begin
  if AMessage = nil then
    Exit;

  FillChar(Header, SizeOf(Header), #32);
  Move(pansichar('ZIO1PBUF')^, Header[0], 8);

  FTempStream.Clear;;
  AMessage.SaveToStream(FTempStream);

  FBufferedStream.WriteBuffer(Header[0], 12);

  Value := FTempStream.Size;
  VarintLen := 0;
  while Value >= $80 do
  begin
    VarintBuf[VarintLen] := byte((Value and $7F) or $80);
    Value := Value shr 7;
    Inc(VarintLen);
  end;
  VarintBuf[VarintLen] := byte(Value);
  Inc(VarintLen);

  FBufferedStream.WriteBuffer(VarintBuf[0], VarintLen);
  if FTempStream.Size > 0 then
    FBufferedStream.WriteBuffer(FTempStream.Memory^, FTempStream.Size);

  Inc(FBytesWritten, 12 + VarintLen + FTempStream.Size);
end;

function TZioStream.ReadMessage(AMessage: TBaseMessage): boolean;
var
  Reader: TProtoStreamReader;
  Header: array[0..11] of ansichar;
  MsgSize: uint32;
begin
  Result := False;

  if (AMessage = nil) or (FBufferedStream.Position + 12 > FBufferedStream.Size) then
    Exit;

  FBufferedStream.ReadBuffer(Header[0], 12);

  if not CompareMem(@Header[0], pansichar('ZIO1PBUF'), 8) then
    Exit;

  Reader := TProtoStreamReader.Create(FBufferedStream, False);
  MsgSize := Reader.ReadVarUInt32;

  if FBufferedStream.Position + MsgSize > FBufferedStream.Size then
    Exit;

  Result := TBaseMessageCracker(AMessage).LoadFromStream(Reader, MsgSize);
  Reader.Free;
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
  ForceDirectories(FPath);

  Pattern := TPattern.Create(FPath, FNumStreams);
  for i := 0 to ANumStreams - 1 do
  begin
    ShardPath := Pattern.GetShardPath(i);
    FileStream := TRobustFileStream.Create(ShardPath, fmCreate);
    BufferedStream := TWriteBufStream.Create(FileStream, 65536);
    ZioStream := TZioStream.Create(FileStream, BufferedStream, True);
    Add(ZioStream);
  end;
  Pattern.Free;
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
  ForceDirectories(FPath);

  for i := 0 to APattern.NumShards - 1 do
  begin
    ShardPath := APattern.GetShardPath(i);
    FileStream := TRobustFileStream.Create(ShardPath, fmCreate);
    BufferedStream := TWriteBufStream.Create(FileStream, 65536);
    ZioStream := TZioStream.Create(FileStream, BufferedStream, True);
    Add(ZioStream);
  end;
end;

destructor TZioStreams.Destroy;
var
  ZioStream: TZioStream;
begin
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

  FPartPaths.Sort;

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
    FileStream := TFileStream.Create(PartPath, fmOpenRead);
    BufferedStream := TReadBufStream.Create(FileStream, FBufferSize);
    FCurrentStream := TZioStream.Create(FileStream, BufferedStream, True);
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

  while FCurrentStream <> nil do
  begin
    if FCurrentStream.ReadMessage(AMessage) then
      Exit(True);

    Inc(FCurrentPartIndex);
    if HasMoreParts then
      OpenNextPart
    else
      Break;
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

  FShardReaders := TZioShardReaderList.Create(True);
  FCurrentShardIndex := 0;
  FBufferSize := ABufferSize;
  FPattern := nil;

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

  FShardReaders := TZioShardReaderList.Create(True);
  FCurrentShardIndex := 0;
  FPattern := APattern;
  FBufferSize := ABufferSize;

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
  FShardReaders.Free;
  inherited Destroy;
end;

function TZioReader.ReadMessage(var AMessage: T): boolean;
begin
  Result := False;

  while FCurrentShardIndex < FShardReaders.Count do
  begin
    if FShardReaders[FCurrentShardIndex].ReadMessage(AMessage) then
      Exit(True);

    Inc(FCurrentShardIndex);
  end;
end;

function TZioReader.ReadMessageFromShard(ShardIndex: integer; var AMessage: T): boolean;
begin
  if (ShardIndex < 0) or (ShardIndex >= FShardReaders.Count) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)',
      [ShardIndex, FShardReaders.Count - 1]);

  Result := FShardReaders[ShardIndex].ReadMessage(AMessage);
end;

function TZioReader.GetTotalStreams: integer;
begin
  Result := FShardReaders.Count;
end;

{ TZioWriter.TZioPartWriter }

constructor TZioWriter.TZioPartWriter.Create(AShardWriter: TZioShardWriter;
  ABufferSize: integer; AMaxPartSize: int64);
begin
  inherited Create;
  FShardWriter := AShardWriter;
  FBufferSize := ABufferSize;
  FMaxPartSize := AMaxPartSize;
  FZioStream := nil;
  FPartPath := '';
end;

constructor TZioWriter.TZioPartWriter.Create(const APartPath: ansistring;
  ABufferSize: integer);
var
  FileStream: TFileStream;
  BufferedStream: TWriteBufStream;
begin
  inherited Create;
  FShardWriter := nil;
  FBufferSize := ABufferSize;
  FMaxPartSize := 0;
  FPartPath := APartPath;

  FileStream := TRobustFileStream.Create(APartPath, fmCreate);
  BufferedStream := TWriteBufStream.Create(FileStream, ABufferSize);
  FZioStream := TZioStream.Create(FileStream, BufferedStream, True);
end;

destructor TZioWriter.TZioPartWriter.Destroy;
begin
  CloseCurrentPart;
  inherited Destroy;
end;

procedure TZioWriter.TZioPartWriter.OpenNextPart;
var
  FileStream: TFileStream;
  BufferedStream: TWriteBufStream;
begin
  CloseCurrentPart;

  if FShardWriter <> nil then
  begin
    FPartPath := FShardWriter.GeneratePartPath;
    FileStream := TRobustFileStream.Create(FPartPath, fmCreate);
    BufferedStream := TWriteBufStream.Create(FileStream, FBufferSize);
    FZioStream := TZioStream.Create(FileStream, BufferedStream, True);
  end;
end;

procedure TZioWriter.TZioPartWriter.CloseCurrentPart;
begin
  if FZioStream <> nil then
    FreeAndNil(FZioStream);
end;

procedure TZioWriter.TZioPartWriter.WriteMessage(AMessage: T);
begin
  if AMessage = nil then
    Exit;

  if FZioStream = nil then
    OpenNextPart;

  FZioStream.WriteMessage(AMessage);

  if (FMaxPartSize > 0) and (FZioStream.BytesWritten >= FMaxPartSize) then
    CloseCurrentPart;
end;

function TZioWriter.TZioPartWriter.GetBytesWritten: int64;
begin
  if FZioStream <> nil then
    Result := FZioStream.BytesWritten
  else
    Result := 0;
end;

{ TZioWriter.TZioShardWriter }

constructor TZioWriter.TZioShardWriter.Create(AParentWriter: TZioWriter;
  AShardIndex: integer; ABufferSize: integer);
begin
  inherited Create;

  FParentWriter := AParentWriter;
  FShardIndex := AShardIndex;
  FSequenceCounter := 0;
  FBufferSize := ABufferSize;
  // TODO: Should we move this to ParentWriter
  FLock := TCriticalSection.Create;

  // Immediately open the part file during constructor phase
  FCurrentPartWriter := NewPartWriter;
  FCurrentPartWriter.OpenNextPart;
end;

destructor TZioWriter.TZioShardWriter.Destroy;
begin
  if FCurrentPartWriter <> nil then
    FCurrentPartWriter.Free;

  FLock.Free;
  inherited Destroy;
end;

function TZioWriter.TZioShardWriter.GeneratePartPath: ansistring;
var
  ShardDir: ansistring;
  Seq: integer;
begin
  // InterlockedIncrement safely adds 1 across all threads and returns the NEW value.
  // We subtract 1 to get the 0-indexed sequence for this specific call.
  Seq := InterlockedIncrement(FSequenceCounter) - 1;

  ShardDir := FParentWriter.FPattern.GetShardPath(FShardIndex);

  Result := Format('%s%spart-%6.6d-%d.zio', [ShardDir, PathDelim,
    Seq, FParentWriter.Timestamp]);
end;

function TZioWriter.TZioShardWriter.NewPartWriter: TZioPartWriter;
begin
  Result := TZioPartWriter.Create(Self, FBufferSize, FParentWriter.MaxPartSize);
end;

procedure TZioWriter.TZioShardWriter.WriteMessage(AMessage: T);
begin
  if AMessage = nil then
    Exit;

  if FCurrentPartWriter = nil then
    FCurrentPartWriter := NewPartWriter;

  FCurrentPartWriter.WriteMessage(AMessage);
end;

{ TZioWriter.TZioPartWriterList }

function TZioWriter.TZioPartWriterList.WriteMessage(Key: UInt32; Message: T
  ): Boolean;
begin
  Self[Key mod Count].WriteMessage(Message);

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

{ TZioWriter }

constructor TZioWriter.Create(APattern: TPattern; ABufferSize: integer = 65536;
  AMaxPartSize: int64 = DEFAULT_MAX_PART_SIZE);
var
  DT: TDateTime;
  i: integer;
begin
  inherited Create;

  DT := Now;
  FTimestamp := DateTimeToUnix(DT, False) * 1000 + MilliSecondOf(DT);
  FMaxPartSize := AMaxPartSize;
  FLock := TCriticalSection.Create;

  FPattern := TPattern.Create(APattern);
  FBufferSize := ABufferSize;
  FCurrentShardIndex := 0;
  FShardWriters := TZioShardWriterList.Create(True);
  FShardWriters.Capacity := FPattern.NumShards;

  for i := 0 to FPattern.NumShards - 1 do
    ForceDirectories(FPattern.GetShardPath(i));

  for i := 0 to FPattern.NumShards - 1 do
    GetOrCreateShardWriter(i);
end;

destructor TZioWriter.Destroy;
begin
  FShardWriters.Free;
  FPattern.Free;
  FLock.Free;
  inherited Destroy;
end;

function TZioWriter.GetOrCreateShardWriter(ShardIndex: integer): TZioShardWriter;
begin
  if (ShardIndex < 0) or (ShardIndex >= FPattern.NumShards) then
    raise EShardException.CreateFmt('Invalid shard index: %d (must be 0..%d)',
      [ShardIndex, FPattern.NumShards - 1]);

  FLock.Acquire;
  while FShardWriters.Count <= ShardIndex do
    FShardWriters.Add(nil);

  if FShardWriters[ShardIndex] = nil then
    FShardWriters[ShardIndex] := TZioShardWriter.Create(Self, ShardIndex, FBufferSize);

  Result := FShardWriters[ShardIndex];
  FLock.Release;
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
  FLock.Acquire;
  ShardIndex := FCurrentShardIndex;
  FCurrentShardIndex := (FCurrentShardIndex + 1) mod FPattern.NumShards;
  FLock.Release;

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
