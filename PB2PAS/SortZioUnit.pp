unit SortZioUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bufstream, ProtoStreamUnit, ZIOStreamUnit, Variants, Math;

type
  { Basic protobuf wire types for field 1 }
  TProtoBasicType = (
    pbtVarInt,      // int32, int64, uint32, uint64, sint32, sint64, bool, enum
    pbtFixed64,     // fixed64, sfixed64, double
    pbtString,      // string
    pbtBytes,       // bytes
    pbtFixed32      // fixed32, sfixed32, float
  );

  { Exception for sort operations }
  ESortZioException = class(Exception);

{ Sort a single ZIO file by field number 1 }
procedure SortZioFile(const InputPath, OutputPath: AnsiString; FieldNumberOneType: TProtoBasicType);

{ Sort sharded ZIO files by field number 1 }
procedure SortZioFiles(InputPattern, OutputPattern: TPattern; FieldNumberOneType: TProtoBasicType);

{ TODO: Implement external merge sort for datasets larger than RAM
  
  For files that don't fit in memory, implement:
  1. Phase 1: Split input into sorted runs that fit in memory
     - Read chunks of records (e.g., 100MB at a time)
     - Sort each chunk in memory
     - Write sorted chunks to temporary files
  
  2. Phase 2: K-way merge of sorted runs
     - Open all temporary sorted files
     - Use priority queue/heap to merge
     - Write merged output to final destination
  
  Algorithm:
    function ExternalMergeSortZioFile(InputPath, OutputPath: string; 
                                       FieldType: TProtoBasicType;
                                       MemoryLimit: Int64 = 1GB)
    
    - CreateSortedRuns(InputPath, TempDir, MemoryLimit) -> TStringList of run files
    - MergeSortedRuns(RunFiles, OutputPath)
    
  Benefits:
    - Can sort arbitrarily large files
    - Configurable memory limit
    - Only small overhead (2 passes over data)
    
  Similar pattern for SortZioFiles with sharded inputs.
}

implementation

type
  { Record to hold sort key and message data }
  TSortRecord = record
    SortKey: Variant;      // The value of field 1
    MessageData: TBytes;   // The entire message as bytes
  end;
  
  TSortRecordArray = array of TSortRecord;

{ Read bytes from reader }
function ReadBytesFromReader(Reader: TProtoStreamReader): TBytes;
var
  Len: UInt32;
begin
  Len := Reader.ReadVarUInt32;
  SetLength(Result, Len);
  if Len > 0 then
    Reader.Position := Reader.Position + Len;  // Skip the bytes, we just store length
end;

{ Extract field 1 from protobuf wire format without deserializing entire message }
function ExtractField1(Reader: TProtoStreamReader; MsgSize: UInt32; 
                       FieldType: TProtoBasicType; out SortKey: Variant): Boolean;
var
  StartPos, EndPos: Int64;
  Tag, FieldNumber, WireType: UInt32;
  Len: UInt32;
begin
  Result := False;
  StartPos := Reader.Position;
  EndPos := StartPos + MsgSize;
  
  // Parse wire format to find field 1
  while Reader.Position < EndPos do
  begin
    Tag := Reader.ReadVarUInt32;
    FieldNumber := Tag >> 3;
    WireType := Tag and $07;
    
    if FieldNumber = 1 then
    begin
      // Found field 1! Read based on expected type
      case FieldType of
        pbtVarInt:
          SortKey := Reader.ReadVarInt64;
        pbtFixed64:
          SortKey := Reader.ReadFixed64;
        pbtString:
          SortKey := Reader.ReadString;
        pbtBytes:
          SortKey := ReadBytesFromReader(Reader);
        pbtFixed32:
          SortKey := Reader.ReadFixed32;
      end;
      
      // Skip rest of message
      Reader.Position := EndPos;
      Result := True;
      Exit;
    end
    else
    begin
      // Skip this field based on wire type
      case WireType of
        0: Reader.ReadVarUInt64;                         // Varint
        1: Reader.Position := Reader.Position + 8;       // Fixed64
        2: begin                                         // Length-delimited
            Len := Reader.ReadVarUInt32;
            Reader.Position := Reader.Position + Len;
          end;
        5: Reader.Position := Reader.Position + 4;       // Fixed32
      end;
    end;
  end;
  
  // Field 1 not found - seek to end of message
  Reader.Position := EndPos;
end;

{ Comparison function for sorting }
function CompareSortRecords(const A, B: TSortRecord): Integer;
var
  BytesA, BytesB: TBytes;
  MinLen, i: Integer;
begin
  // Handle different variant types
  if VarIsNumeric(A.SortKey) and VarIsNumeric(B.SortKey) then
  begin
    if A.SortKey < B.SortKey then
      Result := -1
    else if A.SortKey > B.SortKey then
      Result := 1
    else
      Result := 0;
  end
  else if VarIsStr(A.SortKey) and VarIsStr(B.SortKey) then
  begin
    Result := CompareStr(AnsiString(A.SortKey), AnsiString(B.SortKey));
  end
  else if VarIsArray(A.SortKey) and VarIsArray(B.SortKey) then
  begin
    // Byte array comparison (lexicographic)
    BytesA := A.SortKey;
    BytesB := B.SortKey;
    MinLen := Min(Length(BytesA), Length(BytesB));
    
    Result := 0;
    for i := 0 to MinLen - 1 do
    begin
      if BytesA[i] < BytesB[i] then
      begin
        Result := -1;
        Exit;
      end
      else if BytesA[i] > BytesB[i] then
      begin
        Result := 1;
        Exit;
      end;
    end;
    
    // If all bytes equal up to MinLen, shorter array comes first
    if Length(BytesA) < Length(BytesB) then
      Result := -1
    else if Length(BytesA) > Length(BytesB) then
      Result := 1;
  end
  else
    Result := 0; // Fallback for mixed types
end;

{ QuickSort implementation for TSortRecordArray }
procedure QuickSort(var Arr: TSortRecordArray; L, R: Integer);
var
  I, J: Integer;
  P, Temp: TSortRecord;
begin
  if L >= R then Exit;
  
  repeat
    I := L;
    J := R;
    P := Arr[(L + R) shr 1];
    
    repeat
      while CompareSortRecords(Arr[I], P) < 0 do Inc(I);
      while CompareSortRecords(Arr[J], P) > 0 do Dec(J);
      
      if I <= J then
      begin
        Temp := Arr[I];
        Arr[I] := Arr[J];
        Arr[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    
    if L < J then QuickSort(Arr, L, J);
    L := I;
  until I >= R;
end;

{ Main sorting procedure }
procedure SortZioFile(const InputPath, OutputPath: AnsiString; FieldNumberOneType: TProtoBasicType);
var
  InputFileStream: TFileStream;
  InputBufferedStream: TReadBufStream;
  OutputFileStream: TFileStream;
  OutputBufferedStream: TWriteBufStream;
  Reader: TProtoStreamReader;
  Writer: TProtoStreamWriter;
  Records: TSortRecordArray;
  Count, Capacity: Integer;
  Header: array[0..11] of AnsiChar;
  Magic: string[8];
  MsgSize: UInt32;
  SortKey: Variant;
  MessageBytes: TBytes;
  MessageStartPos: Int64;
  i: Integer;
begin
  if not FileExists(InputPath) then
    raise ESortZioException.CreateFmt('Input file not found: %s', [InputPath]);
  
  // Open input file
  InputFileStream := TFileStream.Create(InputPath, fmOpenRead);
  InputBufferedStream := TReadBufStream.Create(InputFileStream, 131072); // 128KB read buffer
  
  // Phase 1: Read all records and extract field 1 + store message bytes
  Count := 0;
  Capacity := 1000;
  SetLength(Records, Capacity);
  
  while InputBufferedStream.Position < InputBufferedStream.Size do
  begin
    // Read ZIO header (12 bytes)
    if InputBufferedStream.Position + 12 > InputBufferedStream.Size then
      Break;
    
    InputBufferedStream.ReadBuffer(Header[0], 12);
    
    // Validate magic
    SetLength(Magic, 8);
    Move(Header[0], Magic[1], 8);
    if Magic <> 'ZIO1PBUF' then
      raise ESortZioException.CreateFmt('Invalid ZIO magic at position %d', [InputBufferedStream.Position - 12]);
    
    // Read message size
    Reader := TProtoStreamReader.Create(InputBufferedStream, False);
    MsgSize := Reader.ReadVarUInt32;
    
    // Check bounds
    if InputBufferedStream.Position + MsgSize > InputBufferedStream.Size then
      Break;
    
    // Save position before reading message
    MessageStartPos := InputBufferedStream.Position;
    
    // Extract field 1 (partial parse - optimization!)
    if not ExtractField1(Reader, MsgSize, FieldNumberOneType, SortKey) then
      raise ESortZioException.CreateFmt('Field 1 not found in message at position %d', [MessageStartPos]);
    
    Reader.Free;
    
    // Read entire message into byte array
    SetLength(MessageBytes, MsgSize);
    InputBufferedStream.Position := MessageStartPos;
    InputBufferedStream.ReadBuffer(MessageBytes[0], MsgSize);
    
    // Grow array if needed
    if Count >= Capacity then
    begin
      Capacity := Capacity * 2;
      SetLength(Records, Capacity);
    end;
    
    // Store record
    Records[Count].SortKey := SortKey;
    Records[Count].MessageData := MessageBytes;
    Inc(Count);
  end;
  
  SetLength(Records, Count);
  InputBufferedStream.Free;
  InputFileStream.Free;
  
  WriteLn(Format('Read %d records from %s', [Count, InputPath]));
  
  // Phase 2: Sort records by field 1
  WriteLn('Sorting records...');
  if Count > 0 then
    QuickSort(Records, 0, Count - 1);
  
  // Phase 3: Write sorted records to output
  WriteLn(Format('Writing sorted records to %s', [OutputPath]));
  ForceDirectories(ExtractFileDir(OutputPath));
  OutputFileStream := TFileStream.Create(OutputPath, fmCreate);
  OutputBufferedStream := TWriteBufStream.Create(OutputFileStream, 65536); // 64KB write buffer
  
  for i := 0 to Count - 1 do
  begin
    // Write ZIO header
    FillChar(Header, SizeOf(Header), #32);
    Move(PAnsiChar('ZIO1PBUF')^, Header[0], 8);
    OutputBufferedStream.WriteBuffer(Header[0], 12);
    
    // Write message size
    Writer := TProtoStreamWriter.Create(OutputBufferedStream, False);
    Writer.WriteRawVarint32(Length(Records[i].MessageData));
    Writer.Free;
    
    // Write message data
    OutputBufferedStream.WriteBuffer(Records[i].MessageData[0], Length(Records[i].MessageData));
  end;
  
  OutputBufferedStream.Free;
  OutputFileStream.Free;
  
  WriteLn(Format('Successfully wrote %d sorted records', [Count]));
end;

{ Sort sharded ZIO files - per-shard sort }
procedure SortZioFiles(InputPattern, OutputPattern: TPattern; FieldNumberOneType: TProtoBasicType);
var
  InputPaths, OutputPaths: TAnsiStringArray;
  i: Integer;
begin
  // Simply call SortZioFile for each shard independently.
  // Each shard is sorted on its own - this is NOT a global sort across all shards.
  // For global sort (all shards combined), you would need to:
  //   1. Concatenate all input shards into a temp file
  //   2. Call SortZioFile on the temp file
  //   3. Split the sorted temp file back into output shards
  
  InputPaths := InputPattern.GetAllPaths;
  OutputPaths := OutputPattern.GetAllPaths;
  
  if InputPattern.NumShards <> OutputPattern.NumShards then
    raise ESortZioException.CreateFmt(
      'Input and output patterns must have same number of shards: %d vs %d',
      [InputPattern.NumShards, OutputPattern.NumShards]);
  
  WriteLn(Format('Sorting %d sharded ZIO files (per-shard sort)', [InputPattern.NumShards]));
  
  for i := 0 to High(InputPaths) do
  begin
    WriteLn(Format('Sorting shard %d/%d: %s -> %s', 
      [i + 1, Length(InputPaths), InputPaths[i], OutputPaths[i]]));
    SortZioFile(InputPaths[i], OutputPaths[i], FieldNumberOneType);
  end;
  
  WriteLn(Format('Successfully sorted %d shards', [Length(InputPaths)]));
end;

end.
