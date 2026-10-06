unit ZioExternalSorterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PatternUnit, Generics.Collections, Generics.Defaults, DelimitedReaderUnit, ProtoHelperUnit;

type
  { Generic ZIO External Sorter }

  { TZioExternalSorter }

  generic TZioExternalSorter<TMessage: TBaseMessage> = class
  private type
    TKey = AnsiString;
    TData = record
      Key: TKey;
      PayloadLen: Cardinal;
      PayloadData: PByte;
    end;
  public type
    IKeyGenerator = interface
      function Compare(const AMessage: TMessage): AnsiString;
    end;
    TReader = specialize TDelimitedReader<TBaseMessage>;

  private
    FArena: PByte;
    FArenaCapacity: NativeInt;
    FArenaOffset: NativeInt;

    FRecords: specialize TList<TData>;
    FRecordCount: Integer;

    FKeyGen: IKeyGenerator;

    procedure DoSort(L, R: Integer);
    procedure FlushChunk(const OutputFilename: string);
    procedure ResetArena;

    procedure AddRecord(const AKey: TKey; APayload: PByte; APayloadLen: Cardinal);
  public
    constructor Create(AMemoryLimitBytes: NativeInt; KeyGen: IKeyGenerator);
    destructor Destroy; override;

    procedure Sort(const InputPattern, OutputPattern: TPattern);
    procedure Flush;
  end;

implementation

constructor TZioExternalSorter.Create(AMemoryLimitBytes: NativeInt;
  KeyGen: IKeyGenerator);
begin
  FArenaCapacity := AMemoryLimitBytes;
  FArenaOffset := 0;
  GetMem(FArena, FArenaCapacity);

  SetLength(FRecords, 1000000);
  FRecordCount := 0;

  FKeyGen := KeyGen;
end;

destructor TZioExternalSorter.Destroy;
begin
  if FArena <> nil then
    FreeMem(FArena);
  SetLength(FRecords, 0);
  inherited Destroy;
end;

procedure TZioExternalSorter.Sort(const InputPattern, OutputPattern: TPattern);
var
  Message: TMessage;
begin
{  Message := TMessage.Create;

  while Reader.ReadMessage(Message) do
  begin
    Self.AddRecord(Message.);
    Message.Clear;

  end;

  Message.Free;
}
end;

procedure TZioExternalSorter.DoSort(L, R: Integer);
var
  I, J: Integer;
  Pivot, Temp: TData;
begin
  {
  if L >= R then Exit;

  repeat
    I := L;
    J := R;
    // Pick the middle element as pivot
    Pivot := FRecords[L + (R - L) shr 1];

    repeat
      while FComparer.Compare(FRecords[I], Pivot) < 0 do Inc(I);
      while FComparer.Compare(FRecords[J], Pivot) > 0 do Dec(J);

      if I <= J then
      begin
        // Swap records
        Temp := FRecords[I];
        FRecords[I] := FRecords[J];
        FRecords[J] := Temp;

        Inc(I);
        Dec(J);
      end;
    until I > J;

    // Recursively sort the left sub-array
    if L < J then DoSort(L, J);

    // Loop back to sort the right sub-array (tail call optimization)
    L := I;
  until I >= R;
  }
end;

procedure TZioExternalSorter.FlushChunk(const OutputFilename: string);
begin

end;

procedure TZioExternalSorter.ResetArena;
begin
  FArenaOffset := 0;
  FRecordCount := 0;
end;

procedure TZioExternalSorter.AddRecord(const AKey: TKey; APayload: PByte; APayloadLen: Cardinal);
begin
  {
  // Check if Arena or Record array is full; flush if needed
  if (FArenaOffset + APayloadLen > FArenaCapacity) or (FRecordCount >= Length(FRecords)) then
    Flush;

  // Copy payload to continuous Arena block
  Move(APayload^, (FArena + FArenaOffset)^, APayloadLen);

  // Store metadata
  FRecords[FRecordCount].Key := AKey;
  FRecords[FRecordCount].PayloadLen := APayloadLen;
  FRecords[FRecordCount].PayloadData := FArena + FArenaOffset;

  Inc(FArenaOffset, APayloadLen);
  Inc(FRecordCount);
  }
end;

procedure TZioExternalSorter.Flush;
begin
  if FRecordCount = 0 then Exit;

  // In-memory sort of current chunk using our internal QuickSort
  if FRecordCount > 1 then
    DoSort(0, FRecordCount - 1);

  // Write sorted chunk to temp file ...

  ResetArena;
end;

end.
