unit TextCompressionUnit;
{$ASSERTIONS on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CompressionBaseUnit, csvdocument, fgl, UTF8Unit, md5;

type
  TCompressionMode = (cmNoCompression = 0, cmHuffman = 1);

  { TBitStream }

  TBitStream = class(TObject)
  private
    FStream: TStream;
    CurrentByte: Byte;
    CurrentIndex: Integer;
    function GetPosition: Integer;
    function GetSize: Int64;

  public
    property Position: Integer read GetPosition;
    property Size: Int64 read GetSize;
    // Takes the ownership of AnStream Object.
    constructor Create(AnStream: TStream);
    destructor Destroy;

    function ReadNextBit: Integer;
    function ReadNextByte: Integer;

  end;

  { THuffmanTable }

  THuffmanTable = class(specialize TFPGMap<TRune, AnsiString>)
  private
    type

    { THuffmanTableNode }

    THuffmanTableNode = class(TObject)
    private
      FLeft: THuffmanTableNode;
      FRight: THuffmanTableNode;
      FRune: TRune;

      procedure AddData(const Path: AnsiString; Value: TRune);
    public
      property Rune: TRune read FRune;
      property Left: THuffmanTableNode read FLeft;
      property Right: THuffmanTableNode read FRight;

      constructor Create(ARune: TRune);
      constructor Create;
      destructor Destroy; override;

    end;

  private
    FileCount: Integer;
    EncodingID: TMD5Digest;
    RuneCountMap: specialize TFPGMap<TRune, Integer>;
    ReverseMapRoot: THuffmanTableNode;

    function GetCodeForRune(aRune: TRune): AnsiString;
    procedure CreateHuffmanTable;

    function GetRuneByCode(Stream: TBitStream): TRune;
    procedure InitRootNode;

  public
    property CodeForRune[aRune: TRune]: AnsiString read GetCodeForRune;

    // Load the table from file.
    constructor LoadFromFile(const Filename: AnsiString);
    // Load the table from file.
    constructor CreateFromFiles(const RootDir, FilePattern: AnsiString;
      Recursive: Boolean = True);
    destructor Destroy; override;

    procedure SaveToFile(const Filename: AnsiString);
  end;

  { TUnicodeStringArchiver }
  // This class is not Threadsafe.

  TUnicodeStringArchiver = class(TBaseArchiverStream)
  private
    FHTable: THuffmanTable;
    AllRunes: specialize TFPGList<TRune>;

  public
    // Object takes the ownership of the HTable object.
    constructor Create(HTable: THuffmanTable);
    destructor Destroy; override;

    // Returns the diff in size.
    function Compress(IStream, OStream: TStream): Integer; override;
    // Returns the diff in size.
    function DeCompress(IStream, OStream: TStream): Integer; override;
  end;

implementation
uses
  LazFileUtils, HeapUnit;

{ TBitStream }

function TBitStream.GetPosition: Integer;
begin
  Result := FStream.Position * 8 - (8 - CurrentIndex);
end;

function TBitStream.GetSize: Int64;
begin
  Result := FStream.Size * 8;
end;

constructor TBitStream.Create(AnStream: TStream);
begin
  inherited Create;

  FStream := AnStream;
  CurrentIndex := 0;
  FStream.Read(CurrentByte, 1);
end;

destructor TBitStream.Destroy;
begin
  FStream.Free;

  inherited;

end;

function TBitStream.ReadNextBit: Integer;
begin
  Result := (CurrentByte shr CurrentIndex) and 1;
  Inc(CurrentIndex);
  if (CurrentIndex = 8) and (FStream.Position < FStream.Size) then
  begin
    CurrentIndex := 0;
    FStream.ReadBuffer(CurrentByte, 1);

  end;
end;

function TBitStream.ReadNextByte: Integer;
var
  i: Integer;
  P2: Integer;
begin
  Result := 0;
  P2 := 1;
  for i := 0 to 7 do
  begin
    if ReadNextBit = 1 then
      Result := Result or P2;
    Inc(P2, P2);
  end;
end;

{ THuffmanTable.THuffmanTableNode }

procedure THuffmanTable.THuffmanTableNode.AddData(const Path: AnsiString;
  Value: TRune);

  procedure RecAddData(Node: THuffmanTableNode; Index: Integer);
  var
    Next: THuffmanTableNode;

  begin
    if Index = Length(Path) + 1 then
    begin
      Node.FRune :=  Value;
      Assert(Node.Left = nil);
      Assert(Node.Right = nil);
      Exit;
    end;

    if Path[Index] = '0' then
    begin
      if Node.Left = nil then
        Node.FLeft := THuffmanTableNode.Create;
      Next := Node.Left;
    end
    else if Path[Index] = '1' then
    begin
      if Node.Right = nil then
        Node.FRight := THuffmanTableNode.Create;
        Next := Node.Right;
    end;

    RecAddData(Next, Index + 1);
  end;

begin
  RecAddData(Self, 1);

end;

constructor THuffmanTable.THuffmanTableNode.Create(ARune: TRune);
begin
  inherited Create;

  FLeft := nil;
  FRight := nil;
  FRune := ARune;
end;

constructor THuffmanTable.THuffmanTableNode.Create;
begin
  inherited Create;

  FLeft := nil;
  FRight := nil;
  FRune := 0;

end;

destructor THuffmanTable.THuffmanTableNode.Destroy;
begin
  inherited Destroy;
end;

{ THuffmanTable }

function THuffmanTable.GetCodeForRune(aRune: TRune): AnsiString;
begin
  Result := '';
  if not Self.TryGetData(aRune, Result) then
    WriteLn(Format('No code for Rune(%d)!', [aRune]));

end;

constructor THuffmanTable.LoadFromFile(const Filename: AnsiString);
  function ParseEncodingID(const S: AnsiString): TMD5Digest;
  var
    StrList: TStringList;
    i: Integer;
  begin
    StrList := TStringList.Create;
    StrList.Delimiter:= ':';
    StrList.DelimitedText := S;

    for i := 0 to StrList.Count - 1 do
      Result[i] := StrToInt(StrList[i]);

  end;

var
  Document: TCSVDocument;
  i: Integer;

begin
  inherited Create;

  RuneCountMap := specialize TFPGMap<TRune, Integer>.Create;

  Document := TCSVDocument.Create;
  Document.LoadFromFile(Filename);
  FileCount := -1;
  FillChar(EncodingID, SizeOf(EncodingID), 0);

  for i := 0 to Document.RowCount - 1 do
  begin
    if Document.ColCount[i] < 3 then
      Continue;

    if Document.Cells[0, i] = 'Rune Code' then
      Continue;
    if Copy(Document.Cells[0, i], 1, 2) = '--' then
    begin
      case Document.Cells[0, i][16] of
        'F': //--SPECIALTOKEN_FILECOUNT
          FileCount:= StrToInt(Document.Cells[1, i]);
        'E': //--SPECIALTOKEN_ENCODING
          EncodingID := ParseEncodingID(Document.Cells[1, i])
        else
          WriteLn(Format('Errror Cell1 = %s Cell2 = %s Cell3= %s',
             [Document.Cells[0, i],
              Document.Cells[1, i],
              Document.Cells[2, i]]));
      end;

      Continue;
    end;
    Self.Add(StrToInt(Document.Cells[0, i]),
                             Document.Cells[1, i]);
    RuneCountMap.Add(StrToInt(Document.Cells[0, i]),
                     StrToInt(Document.Cells[2, i]));
  end;

  Document.Free;
  InitRootNode;
end;

type
  EInvalidEncoding = class(Exception);

procedure GetAllRunes(const IStream: TStream; out Output: TRuneList);
  function GetNextRune: TRune;
  var
    Len: Integer;
    b1, b2, b3, b4: Byte;
  begin
    IStream.Read(b1, 1);

    if b1 and 128= 0 then
    begin
      Result := b1
    end
    else if b1 and 32 = 0 then
    begin
      IStream.Read(b2, 1);
      b2 := b2 xor 128;
      b1 := b1 xor (128+ 64);
      Result := b2+ b1 shl 6;
    end
    else if b1 and 16 = 0 then
    begin
      IStream.Read(b2, 1);
      IStream.Read(b3, 1);
      b3:= b3 xor 128;
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64+ 32);
      Result := b3 + b2 shl 6 + b1 shl 12;
    end
    else if b1 and 8 = 0 then
    begin
      IStream.Read(b2, 1);
      IStream.Read(b3, 1);
      IStream.Read(b4, 1);

      b4:= b4 xor 128;
      b3:= b3 xor 128;
      b2:= b2 xor 128;
      b1:= b1 xor (128+ 64+ 32+ 16);
      Result := b4+ b3 shl 6+ b2 shl 12+ (b1 shl 18);
    end
    else
      raise EInvalidEncoding. Create('Invalid Char');
  end;


begin
  while IStream.Position < IStream.Size do
    Output.Add(GetNextRune);
end;

type
  TRuneCountMap = specialize TFPGMap<TRune, Integer>;

procedure ProcessAllFiles(RootDir: AnsiString; RuneCountMap: TRuneCountMap;
    var FileCount: Integer);
  procedure CountRunes(IStream: TStream; CountMap: TRuneCountMap);
  var
    AllRunes: TRuneList;
    Rune: TRune;
    Value: Integer;

  begin
    AllRunes := TRuneList.Create;
    GetAllRunes(IStream, AllRunes);

    for Rune in AllRunes do
    begin
      Value := 0;
      CountMap.TryGetData(Rune, Value);
      CountMap.AddOrSetData(Rune, Value + 1);
    end;

    AllRunes.Free;
  end;

var
  Info: TSearchRec;
  InputStream: TFileStream;

begin
  try
    if FindFirst(AppendPathDelim(RootDir) + '*', faAnyFile, Info) = 0 then
    begin
      repeat
        if Info.Attr and faDirectory = faDirectory then
        begin
          if Info.Name[1] <> '.' then
            ProcessAllFiles(
              AppendPathDelim(RootDir) + info.Name,
              RuneCountMap,
              FileCount);
        end
        else if Info.Attr and faHidden = 0 then
        begin
          InputStream := TFileStream.Create(
            AppendPathDelim(RootDir) + info.Name,
            fmOpenRead);

          try
            CountRunes(InputStream, RuneCountMap);

           except on e: EInvalidEncoding do
           begin
             WriteLn(Format('File %s is not encoded as UTF8.', [AppendPathDelim(RootDir) + info.Name]));
             Dec(FileCount);
           end;
          end;
          Inc(FileCount);
          InputStream.Free;

        end;
      until FindNext(Info) <> 0;
    end;
  finally
  end;
end;

constructor THuffmanTable.CreateFromFiles(const RootDir,
  FilePattern: AnsiString; Recursive: Boolean);

  function GenerateEncodingID: TMD5Digest;
  var
    Text: AnsiString;
    i: Integer;
    Key: TRune;

  begin
    Text := '';
    for i := 0 to Self.Count - 1 do
    begin
      Key := Self.Keys[i];
      Text += Format('%d->%s\n', [Key, Self[Key]]);
    end;
    Result := MD5String(Text);
  end;

begin
  inherited Create;

  RuneCountMap := specialize TFPGMap<TRune, Integer>.Create;

  FileCount := 0;
  ProcessAllFiles(RootDir, RuneCountMap, FileCount);

  CreateHuffmanTable;
  InitRootNode;
  EncodingID := GenerateEncodingID;
end;

type

  { TNode }

  TNode = class(TObject)
  private
    Child1, Child2: TNode;
    TotalCount: Integer;
    Rune: TRune;
    Caption: AnsiString;
  public
    constructor Create(Ch1, Ch2: TNode);
    constructor Create(r: TRune; c: Integer);
    destructor Destroy; override;

    procedure Generate(
        Prefix: AnsiString;
        RuneCode: specialize TFPGMap<TRune, AnsiString>);

    function ToString: AnsiString; override;
  end;

function IsGreaterThan(const a, b: TNode): Boolean;
begin
  Result := a.TotalCount - b.TotalCount > 0;
end;

{ TNode }

constructor TNode.Create(Ch1, Ch2: TNode);
begin
  inherited Create;

  Child1 := Ch1;
  Child2 := Ch2;
  TotalCount := Child1.TotalCount + Child2.TotalCount;
  Rune := 0;
  Caption := Ch1.Caption + '-' + Ch2.Caption;

end;

constructor TNode.Create(r: TRune; c: Integer);
begin
  inherited Create;

  Child1 := nil;
  Child2 := nil;
  Rune := r;
  TotalCount := c;
  Caption := IntToStr(Rune);
end;

destructor TNode.Destroy;
begin
  if Self.Child1 = nil then
  begin
    Self.Child1.Free;
    Self.Child2.Free;
  end;

  inherited Destroy;
end;

procedure TNode.Generate(Prefix: AnsiString; RuneCode: specialize TFPGMap<
  TRune, AnsiString>);
begin
  if Self.Child1 = nil then
  begin
    RuneCode.Add(Self.Rune, Prefix);
    Exit;
  end;

  Child1.Generate(Prefix + '0', RuneCode);
  Child2.Generate(Prefix + '1', RuneCode);

end;

function TNode.ToString: AnsiString;
begin
  if Child1 = nil then
    Result := IntToStr(Rune) + ':' + WideChar(Rune) + ':' + IntToStr(TotalCount)
  else
    Result := Caption + IntToStr(TotalCount)
end;

procedure THuffmanTable.CreateHuffmanTable;
var
  HeapData: specialize THeap<TNode>;
  i: Integer;
  Key: TRune;
  N1, N2: TNode;
  Root: TNode;

begin
  HeapData := specialize THeap<TNode>.Create(@IsGreaterThan);
  for i := 0 to RuneCountMap.Count - 1 do
  begin
    Key := RuneCountMap.Keys[i];
    HeapData.Insert(TNode.Create(Key, RuneCountMap[Key]));
  end;

  while HeapData.Count <> 1 do
  begin
    N1 := HeapData.Min;
    HeapData.DeleteMin;
    N2 := HeapData.Min;
    HeapData.DeleteMin;

    HeapData.Insert(TNode.Create(N1, N2));
  end;
  Root := HeapData.Min;
  HeapData.Free;

  Root.Generate('', Self);
  Root.Free;

end;

function THuffmanTable.GetRuneByCode(Stream: TBitStream): TRune;
var
  Current : THuffmanTableNode;
  b: Integer;

begin
  Current := ReverseMapRoot;

  while (Current.Left <> nil) or (Current.Right <> nil) do
  begin
    b := Stream.ReadNextBit;
    if b = 0 then
      Current := Current.Left
    else
      Current := Current.Right;
    Assert(Current <> nil);
  end;

  Result := Current.Rune;
end;

procedure THuffmanTable.InitRootNode;
var
  i: Integer;
  Key: TRune;
  Value: Ansistring;

begin
  ReverseMapRoot := THuffmanTableNode.Create;
  for i := 0 to Self.Count - 1 do
  begin
    Key := Self.Keys[i];
    Value := Self[Key];

    ReverseMapRoot.AddData(Value, Key);
  end;
end;

destructor THuffmanTable.Destroy;
begin
  inherited Destroy;
end;

procedure THuffmanTable.SaveToFile(const Filename: AnsiString);
  function ToString(Digest: TMD5Digest): AnsiString;
  var
    i: Integer;

  begin
    Result := Format('%d', [Digest[0]]);
    for i := 1 to High(Digest) do
      Result += Format(':%d', [Digest[i]]);
  end;

var
  Document: TCSVDocument;
  i: Integer;
  Key: TRune;

begin
  Document := TCSVDocument.Create;
  Document.AddRow('Rune Code');
  Document.AddCell(0, 'HuffmanCode');
  Document.AddCell(0, 'Count');
  Document.AddRow('--SPECIALTOKEN_FILECOUNT');
  Document.AddCell(1, IntToStr(FileCount));
  Document.AddCell(1, '');
  Document.AddRow('--SPECIALTOKEN_ENCODINGID');
  Document.AddCell(2, ToString(EncodingID));
  Document.AddCell(2, '');

  for i := 0 to Self.Count - 1 do
  begin
    Key := Self.Keys[i];
    Document.AddRow(IntToStr(Key));
    Document.AddCell(i + 3, Format('%s', [Self[Key]]));
    Document.AddCell(i + 3, Format('%d', [RuneCountMap[Key]]));
  end;

  Document.SaveToFile(Filename);
  Document.Free;

end;

constructor TUnicodeStringArchiver.Create(HTable: THuffmanTable);
var
  i, j: Integer;

begin
  inherited Create;
  FHTable := HTable;

  AllRunes := specialize TFPGList<TRune>.Create;
end;

destructor TUnicodeStringArchiver.Destroy;
begin
  FHTable.Free;
  AllRunes.Free;

  inherited Destroy;
end;

type
  TByteArray = array of Byte;

procedure WriteData(Buffer: TByteArray; OStream: TStream);
begin
  if OStream.Write(Buffer[0], Length(Buffer)) <> Length(Buffer) then
    WriteLn('ERROR');

end;

var
  OutContent: AnsiString;
const
  p2: array[1..8] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);

function TUnicodeStringArchiver.Compress(IStream, OStream: TStream): Integer;
  function GetCurrentChar(var CurrentPtr: PChar; Len: Integer): Byte;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to Len do
    begin
      if CurrentPtr^ = '1' then
        Result := Result or p2[i];
      Inc(CurrentPtr);
    end;
  end;

var
  Data: array of Byte;
  i: Integer;
  OutputLen: Integer;
  Rune: TRune;
  b: Byte;
  CurrentValue, Value: AnsiString;
  Ch: Char;
  ChrPtr: PChar;

begin
  AllRunes.Clear;
  GetAllRunes(IStream, AllRunes);
  OutputLen := 0;
  for Rune in AllRunes do
  begin
    if FHTable.TryGetData(Rune, CurrentValue) then
      Inc(OutputLen, Length(CurrentValue))
    else
      WriteLn(Format('Rune(%d) not found', [Rune]));
  end;
  if Length(OutContent) < OutputLen then
    SetLength(OutContent, OutputLen);
  SetLength(Data, 4);
  ChrPtr:= PChar(OutContent);

  Result := OStream.Position;
  b := Ord(cmHuffman) * 8 + (OutputLen and 7);
  OStream.Write(b, 1);
  b := FHTable.EncodingID[2];
  OStream.Write(b, 1);

  for Rune in AllRunes do
  begin
    Value := '';
    FHTable.TryGetData(Rune, Value);

    for Ch in Value do
    begin
      ChrPtr^ := Ch;
      Inc(ChrPtr);
    end;
  end;

  ChrPtr:= PChar(OutContent);
  for i := 1 to OutputLen div 8 do
  begin
    b := GetCurrentChar(ChrPtr, 8);
    OStream.Write(b, 1);
  end;
  if OutputLen and $7 <> 0 then
  begin
    b := GetCurrentChar(ChrPtr, OutputLen - 8 * i);
    OStream.Write(b, 1);
  end;

  AllRunes.Free;
  Result := IStream.Size - (OStream.Position - Result);

end;

function TUnicodeStringArchiver.DeCompress(IStream, OStream: TStream): Integer;
var
  IBitStream: TBitStream;
  Rune: TRune;
  BitCount: Integer;
  Data: array of Byte;
  Len, Mode: Integer;
  LMode, b: Integer;

begin
  Result :=  IStream.Position - OStream.Position;

  SetLength(data, 4);
  IBitStream := TBitStream.Create(IStream);
  LMode := IBitStream.ReadNextByte;
  Len := LMode and 7;
  Mode := (LMode and 8) shr 3;
  assert(Mode = 1, IntToStr(Mode));

  b := IBitStream.ReadNextByte;
  Assert(FHTable.EncodingID[2] = b, Format('HTable:%d vs Compressed:%d',
     [FHTable.EncodingID[2], b]));

  while IBitStream.Position < IBitStream.Size - (8 - Len) do
  begin
    Rune := FHTable.GetRuneByCode(IBitStream);
    BitCount := RuneToUTF8(Rune, Data);
    OStream.Write(Data[0], BitCount);

  end;
  Assert(IBitStream.Position = IBitStream.Size - (8 - Len));

  IBitStream.Free;
  SetLength(Data, 0);
  Result :=  OStream.Size - IStream.Size;
end;

end.

