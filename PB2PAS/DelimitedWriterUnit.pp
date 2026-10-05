unit DelimitedWriterUnit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, BufStream, PatternUnit, ProtoStreamUnit,
  ProtoHelperUnit, Generics.Collections;

const
  OneGB = 1000000000;

type
  TIntList = specialize TList<integer>;
  TAnsiStringArray = array of ansistring;

  { TRobustFileStream - Retries partial writes under kernel backpressure }
  TRobustFileStream = class(TFileStream)
  public
    function Write(const Buffer; Count: longint): longint; override;
  end;

  { TDelimitedWriter }
  generic TDelimitedWriter<TMessage: TBaseMessage> = class(TObject)
  public
  type

    { TPartWriter }
    TPartWriter = class(TObject)
    private
      FShardPath: string;
      FTimestampStr: string;
      FWriterID: integer;
      FSequence: integer;
      FMaxPartSize: int64;
      FBufferSize: integer;

      FFileStream: TRobustFileStream;
      FBufferedStream: TWriteBufStream;
      FTempStream: TMemoryStream;
      FBytesWritten: int64;

      procedure OpenNextPart;
      procedure CloseCurrentPart;
      function GeneratePartPath: string;
    public
      constructor Create(const AShardPath, ATimestampStr: string;
        AWriterID: integer; AMaxPartSize: int64; ABufferSize: integer);
      destructor Destroy; override;

      function WriteMessage(AMessage: TMessage): boolean;
      function WriteMessage(const AKey: ansistring; AMessage: TMessage): boolean;

      property BytesWritten: int64 read FBytesWritten;
    end;

    { TPartWriters }

    TPartWriters = class(specialize TObjectList<TPartWriter>)
    public
      function WriteMessage(const AKey: ansistring; AMessage: TMessage): boolean;
    end;

    { TShardWriter }
    TShardWriter = class(TObject)
    private
      FShardIndex: integer;
      FShardPath: string;
      FTimestampStr: string;
      FMaxPartSize: int64;
      FBufferSize: integer;
      FPartZeroWriter: TPartWriter;
    public
      constructor Create(AShardIndex: integer; const AShardPath, ATimestampStr: string;
        AMaxPartSize: int64; ABufferSize: integer);
      destructor Destroy; override;

      procedure WriteMessage(AMessage: TMessage);
      function WriteMessage(const AKey: ansistring; AMessage: TMessage): boolean;
    end;

    TShardWriters = specialize TObjectList<TShardWriter>;

  private
    FPattern: TPattern;
    FMaxPartSize: int64;
    FBufferSize: integer;
    FShards: TShardWriters;
    FTimestampStr: string;
    FNextWorkerID: integer;

    function GetShardWriter(Index: integer): TShardWriter;

  public
    constructor Create(aPattern: TPattern; AMaxPartSize: int64 = OneGB;
      ABufferSize: integer = 65536);
    constructor Create(const ABaseDir, ABaseName: string;
      AMaxPartSize: int64 = OneGB; ABufferSize: integer = 65536);
    destructor Destroy; override;

    property ShardWriter[Index: integer]: TShardWriter read GetShardWriter;

    function GetShardWriters: TShardWriters;
    function NewPartWriters: TPartWriters;

    procedure WriteMessage(ShardIndex: integer; AMessage: TMessage);
    function WriteMessage(const AKey: ansistring; AMessage: TMessage): boolean;
  end;

function HashKey(const Key: ansistring): cardinal;

implementation

{ TRobustFileStream }

function TRobustFileStream.Write(const Buffer; Count: longint): longint;
var
  TotalWritten, BytesWritten: longint;
  P: pbyte;
begin
  TotalWritten := 0;
  P := pbyte(@Buffer);

  while TotalWritten < Count do
  begin
    BytesWritten := FileWrite(Handle, P^, Count - TotalWritten);
    if BytesWritten <= 0 then
      raise EStreamException.Create('Stream write failed or disk full');

    Inc(TotalWritten, BytesWritten);
    Inc(P, BytesWritten);
  end;

  Result := TotalWritten;
end;


{ TDelimitedWriter.TPartWriter }

constructor TDelimitedWriter.TPartWriter.Create(const AShardPath, ATimestampStr: string;
  AWriterID: integer; AMaxPartSize: int64; ABufferSize: integer);
begin
  inherited Create;
  FShardPath := IncludeTrailingPathDelimiter(AShardPath);
  FTimestampStr := ATimestampStr;
  FWriterID := AWriterID;
  FSequence := 0;
  FMaxPartSize := AMaxPartSize;
  FBufferSize := ABufferSize;
  FTempStream := TMemoryStream.Create;

  if not DirectoryExists(FShardPath) then
    ForceDirectories(FShardPath);

  OpenNextPart;
end;

destructor TDelimitedWriter.TPartWriter.Destroy;
begin
  CloseCurrentPart;
  FTempStream.Free;
  inherited Destroy;
end;

function TDelimitedWriter.TPartWriter.GeneratePartPath: string;
begin
  Inc(FSequence);
  // Example: data/shard-0000-of-0016/part-20261003135322-00000-00001.pb
  Result := Format('%spart-%s-%.5d-%.5d.pb', [FShardPath,
    FTimestampStr, FWriterID, FSequence]);
end;

procedure TDelimitedWriter.TPartWriter.OpenNextPart;
begin
  CloseCurrentPart;
  FBytesWritten := 0;
  FFileStream := TRobustFileStream.Create(GeneratePartPath, fmCreate);
  FBufferedStream := TWriteBufStream.Create(FFileStream, FBufferSize);
end;

procedure TDelimitedWriter.TPartWriter.CloseCurrentPart;
begin
  if Assigned(FBufferedStream) then
    FreeAndNil(FBufferedStream);
  if Assigned(FFileStream) then
    FreeAndNil(FFileStream);
end;

function TDelimitedWriter.TPartWriter.WriteMessage(AMessage: TMessage): boolean;
var
  VarintBuf: array[0..9] of byte;
  VarintLen: integer;
  Value: uint64;
begin
  Result := True;
  if AMessage = nil then Exit(False);

  if FBytesWritten >= FMaxPartSize then
    OpenNextPart;

  FTempStream.Clear;
  AMessage.SaveToStream(FTempStream);

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

  FBufferedStream.WriteBuffer(VarintBuf, VarintLen);
  if FTempStream.Size > 0 then
    FBufferedStream.WriteBuffer(FTempStream.Memory^, FTempStream.Size);

  Inc(FBytesWritten, VarintLen + FTempStream.Size);
end;

function TDelimitedWriter.TPartWriter.WriteMessage(const AKey: ansistring;
  AMessage: TMessage): boolean;

  function GetVarintLen(Value: uint64): integer;
  begin
    if Value = 0 then Exit(1);
    Result := 0;
    while Value > 0 do
    begin
      Inc(Result);
      Value := Value shr 7;
    end;
  end;

  function WriteVarintToBuf(Value: uint64; var Buf: array of byte;
    StartIdx: integer): integer;
  var
    idx: integer;
  begin
    idx := StartIdx;
    while Value >= $80 do
    begin
      Buf[idx] := byte((Value and $7F) or $80);
      Value := Value shr 7;
      Inc(idx);
    end;
    Buf[idx] := byte(Value);
    Result := (idx - StartIdx) + 1;
  end;

var
  KeySize, MsgSize, TotalPayloadSize: uint64;
  KeySizeVarintLen, MsgSizeVarintLen, TotalSizeVarintLen: integer;
  VarintBuf: array[0..31] of byte;
  BufLen: integer;
begin
  if AMessage = nil then Exit(False);

  // 1. Serialize the inner message to get its exact size
  FTempStream.Clear;
  AMessage.SaveToStream(FTempStream);

  MsgSize := FTempStream.Size;
  KeySize := Length(AKey);

  KeySizeVarintLen := GetVarintLen(KeySize);
  MsgSizeVarintLen := GetVarintLen(MsgSize);

  // 2. Calculate the exact payload size of the wrapper Protobuf message
  // 1 byte (Tag $0A) + Varint(KeySize) + KeyBytes + 1 byte (Tag $12) + Varint(MsgSize) + MsgBytes
  TotalPayloadSize := 1 + KeySizeVarintLen + KeySize + 1 + MsgSizeVarintLen + MsgSize;

  // File size limit reached? Rotate.
  if FBytesWritten >= FMaxPartSize then
    OpenNextPart;

  // 3. Write Total Wrapper Size (Length-Delimited prefix for the whole object)
  TotalSizeVarintLen := WriteVarintToBuf(TotalPayloadSize, VarintBuf, 0);
  FBufferedStream.WriteBuffer(VarintBuf, TotalSizeVarintLen);

  // 4. Write Field 1 (Key): Tag 10 ($0A)
  VarintBuf[0] := $0A;
  BufLen := 1;
  BufLen := BufLen + WriteVarintToBuf(KeySize, VarintBuf, BufLen);
  FBufferedStream.WriteBuffer(VarintBuf, BufLen);

  if KeySize > 0 then
    FBufferedStream.WriteBuffer(AKey[1], KeySize); // Write Key string bytes

  // 5. Write Field 2 (Message): Tag 18 ($12)
  VarintBuf[0] := $12;
  BufLen := 1;
  BufLen := BufLen + WriteVarintToBuf(MsgSize, VarintBuf, BufLen);
  FBufferedStream.WriteBuffer(VarintBuf, BufLen);

  if MsgSize > 0 then
    FBufferedStream.WriteBuffer(FTempStream.Memory^, MsgSize);
  // Write inner message bytes

  Inc(FBytesWritten, TotalSizeVarintLen + TotalPayloadSize);
  Result := True;
end;

{ TDelimitedWriter.TPartWriters }

function TDelimitedWriter.TPartWriters.WriteMessage(const AKey: ansistring;
  AMessage: TMessage): boolean;
var
  TargetShard: integer;
begin
  // Automatically hash the Key to route it to a consistent shard
  TargetShard := HashKey(AKey) mod cardinal(Self.Count);
  Result := Self[TargetShard].WriteMessage(AKey, AMessage);

end;


{ TDelimitedWriter.TShardWriter }

constructor TDelimitedWriter.TShardWriter.Create(AShardIndex: integer;
  const AShardPath, ATimestampStr: string; AMaxPartSize: int64; ABufferSize: integer);
begin
  inherited Create;
  FShardIndex := AShardIndex;
  FShardPath := AShardPath;
  FTimestampStr := ATimestampStr;
  FMaxPartSize := AMaxPartSize;
  FBufferSize := ABufferSize;

  // Initialize the dedicated WriterID 0 for direct WriteMessage calls
  FPartZeroWriter := TPartWriter.Create(FShardPath, FTimestampStr,
    0, FMaxPartSize, FBufferSize);
end;

destructor TDelimitedWriter.TShardWriter.Destroy;
begin
  FPartZeroWriter.Free;
  inherited Destroy;
end;

procedure TDelimitedWriter.TShardWriter.WriteMessage(AMessage: TMessage);
begin
  FPartZeroWriter.WriteMessage(AMessage);
end;

function TDelimitedWriter.TShardWriter.WriteMessage(const AKey: ansistring;
  AMessage: TMessage): boolean;
begin
  Result := FPartZeroWriter.WriteMessage(AKey, AMessage);

end;


{ TDelimitedWriter }

constructor TDelimitedWriter.Create(aPattern: TPattern; AMaxPartSize: int64;
  ABufferSize: integer);
var
  i: integer;
begin
  inherited Create;
  FPattern := aPattern;
  FMaxPartSize := AMaxPartSize;
  FBufferSize := ABufferSize;
  FShards := TShardWriters.Create(True); // Owns the TShardWriter instances
  FTimestampStr := FormatDateTime('yyyymmddhhnnss', Now);
  FNextWorkerID := 0; // Starts at 0 so the first worker becomes ID 1

  for i := 0 to FPattern.NumShards - 1 do
    FShards.Add(TShardWriter.Create(i, FPattern.GetShardPath(i),
      FTimestampStr, FMaxPartSize, FBufferSize));
end;

constructor TDelimitedWriter.Create(const ABaseDir, ABaseName: string;
  AMaxPartSize: int64; ABufferSize: integer);
var
  CombinedPath: string;
begin
  CombinedPath := IncludeTrailingPathDelimiter(ABaseDir) + ABaseName;
  Create(TPattern.Create(CombinedPath, 1), AMaxPartSize, ABufferSize);
end;

destructor TDelimitedWriter.Destroy;
begin
  FShards.Free;
  FPattern.Free;
  inherited Destroy;
end;

function TDelimitedWriter.GetShardWriter(Index: integer): TShardWriter;
begin
  if (Index < 0) or (Index >= FShards.Count) then
    raise EShardException.CreateFmt('Shard index %d out of bounds', [Index]);
  Result := FShards[Index];
end;

function HashKey(const Key: ansistring): cardinal;
var
  PCh: pchar;
begin
  Result := 5381; // djb2 hash
  PCh := @Key[1];
  while PCh^ <> #0 do
  begin
    Result := ((Result shl 5) + Result) xor Ord(PCh^);
    Inc(PCh);

  end;
end;

function TDelimitedWriter.GetShardWriters: TShardWriters;
begin
  Result := FShards;
end;

function TDelimitedWriter.NewPartWriters: TPartWriters;
var
  WorkerID, i: integer;
begin
  Result := TPartWriters.Create(True); // List owns the TPartWriter instances
  WorkerID := InterlockedIncrement(FNextWorkerID);

  for i := 0 to FPattern.NumShards - 1 do
    Result.Add(TPartWriter.Create(FPattern.GetShardPath(i), FTimestampStr,
      WorkerID, FMaxPartSize, FBufferSize));
end;

procedure TDelimitedWriter.WriteMessage(ShardIndex: integer; AMessage: TMessage);
begin
  GetShardWriter(ShardIndex).WriteMessage(AMessage);
end;

function TDelimitedWriter.WriteMessage(const AKey: ansistring;
  AMessage: TMessage): boolean;
var
  TargetShard: integer;
begin
  Result := True;
  if FShards.Count = 0 then Exit(False);

  // Automatically hash the Key to route it to a consistent shard
  TargetShard := HashKey(AKey) mod cardinal(FShards.Count);
  Result := GetShardWriter(TargetShard).WriteMessage(AKey, AMessage);
end;

end.
