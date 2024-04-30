unit StreamUnit;
{$Mode objfpc}
interface

uses
  Classes, SysUtils;

const
  SpaceChars: set of char = [' '];

type

  { EEoStream }

  EEoStream = class(Exception);

  { TMyTextStream }

  TMyTextStream = class(TObject)
  private
    CurrentLine: AnsiString;
    FTargerStream: TStream;
    FDeleteInputStream: Boolean;
    function GetEoStream: Boolean;
    function GetPosition: Integer;
    function GetSize: Integer;
    procedure SetPosition(Pos: Integer);
  public
    property TargetStream: TStream read FTargerStream;
    property Size: Integer read GetSize;
    property Position: Integer read GetPosition write SetPosition;
    property EoStream: Boolean read GetEoStream;

    function ReadCh: Char;
    function ReadLine: AnsiString;
    function ReadInteger: Integer;
    function ReadAll: AnsiString;
    function ReadTheRest: AnsiString;

    function ReadWideChar: WideChar;
    function ReadLnWideString: WideString;
    function ReadWideString: WideString;

    procedure WriteLine(const S: AnsiString = '');
    procedure WriteChar(Ch: Char);
    procedure WriteStr(const S: AnsiString);

    {
      Does not reset the position of AnStream
    }
    constructor Create(AnStream: TStream; DeleteInputStream: Boolean = False);
    destructor Destroy; override;

  end;

  {
  I am goint to implement it later
  TMyTextFileStream =  class(TMyTextStream)
  private
    FTargerStream: TextFile;
    function GetEoStream: Boolean;
    function GetPosition: Integer;
    function GetSize: Integer;
    procedure SetPosition(Pos: Integer);

  public
    property Size: Integer read GetSize;
    property Position: Integer read GetPosition write SetPosition;
    property EoStream: Boolean read GetEoStream;

    function ReadCh: Char;
    function ReadLine: AnsiString;
    function ReadInteger: Integer;

    function ReadWideChar: WideChar;
    function ReadLnWideString: WideString;
    function ReadWideString: WideString;

    procedure WriteLine(const S: AnsiString);
    procedure WriteChar(Ch: Char);
    procedure WriteStr(const S: AnsiString);

    /*
      Does not reset the position of AnStream
    */
    constructor Create(AnStream: TStream; DeleteInputStream: Boolean =  False);
    destructor Destroy; override;

  end;
}

  { TMyBinStream }

  TMyBinStream = class(TObject)
  private
    FDeleteInputStream: Boolean;
    FTargerStream: TStream;
    function GetEndOfStream: Boolean;
  public
    property TargetStream: TStream read FTargerStream;
    property EndOfStream: Boolean read GetEndOfStream;

    function ReadCh: Char;
    function ReadInt: Integer;
    function ReadInt32: Int32;
    function ReadInt64: Int64;
    function ReadUInt32: UInt32;
    function ReadUInt64: UInt64;
    function ReadByte: Byte;
    function ReadStr: AnsiString;

    procedure WriteChar(const Ch: Char);
    procedure WriteByte(const b: Byte);
    procedure WriteInt(const n: Integer);
    procedure WriteInt32(const n: Int32);
    procedure WriteInt64(const n: Int64);
    procedure WriteUInt32(const n: uInt32);
    procedure WriteUInt64(const n: uInt64);
    procedure WriteStr(const S: AnsiString);

    constructor Create(AnStream: TStream; DeleteInputStream: Boolean = False);
    destructor Destroy; override;

  end;

function ReadFile(aFilename: AnsiString): AnsiString;

implementation

uses
  WideStringUnit, StringUnit, ALoggerUnit;

function ReadFile(aFilename: AnsiString): AnsiString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(aFilename, fmOpenRead);
  Stream.Position := 0;

  SetLength(Result, Stream.Size);

  Stream.Read(Result[1], Stream.Size);

  Stream.Free;
end;

{ TMyStream }

function TMyTextStream.GetPosition: Integer;
begin
  Result := FTargerStream.Position;

end;

function TMyTextStream.GetEoStream: Boolean;
begin
  Exit(Size <= Position);

end;

function TMyTextStream.GetSize: Integer;
begin
  Result := FTargerStream.Size;

end;

procedure TMyTextStream.SetPosition(Pos: Integer);
begin
  FTargerStream.Position := Pos;

end;

function TMyTextStream.ReadCh: Char;
begin
  FTargerStream.Read(Result, 1);

end;

function TMyTextStream.ReadLine: AnsiString;
var
  Ch: Char;
begin
  Result := '';

  while Position < Size do
  begin
    Ch := ReadCh;
    if (Ch = #10) then
      Break;
    Result := Result + Ch;

  end;

  if Result = '' then
    Exit;

  if Result[Length(Result)] = #13 then
    Delete(Result, Length(Result), 1);

end;

function TMyTextStream.ReadInteger: Integer;
const
  IntDigitChar: set of Char = ['0'..'9'];
var
  Ch, Sign: Char;
begin
  Result := 0;

  Ch := ReadCh;
  while (Ch in SpaceChars) and (Position < Size) do
    Ch := ReadCh;
  if Ch in SpaceChars then
    Exit;

  Sign := '+';
  if Ch in ['+', '-'] then
  begin
    Sign := Ch;
    Ch := ReadCh;

  end;

  while (Ch in IntDigitChar) and (Position < Size) do
  begin
    Result := 10 * Result + Ord(Ch) - 48;
    Ch := ReadCh;

  end;

  if not (Ch in IntDigitChar) then
    SetPosition(Position - 1);

  if Sign = '-' then
    Result *= -1;

end;

function TMyTextStream.ReadAll: AnsiString;
begin
  TargetStream.Position := 0;
  Result := '';
  SetLength(Result, TargetStream.Size);
  TargetStream.Read(Result[1], TargetStream.Size);

end;

function TMyTextStream.ReadTheRest: AnsiString;
var
  Lines: TStringList;
  S: AnsiString;
begin
  Lines := TStringList.Create;

  while TargetStream.Position < TargetStream.Size do
  begin
    S := ReadLine;
    Lines.Add(S);
    if Lines.Count mod 10000 = 0 then
    begin
      ALoggerUnit.GetLogger.FMTDebugLn(
        'StreamUnit %d: %d',
        [Lines.Count, (100 * TargetStream.Position) div TargetStream.Size]);
      Break;
    end;

  end;

  Result := JoinStrings(Lines, sLineBreak);
  Lines.Free;

end;

function TMyTextStream.ReadWideChar: WideChar;
var
  c1, c2, c3, c4: Char;
  b1, b2, b3, b4: Byte;
  Value: Integer;
begin
  if FTargerStream.Size <= FTargerStream.Position then
    raise EEoStream.Create('');

  FTargerStream.Read(c1, 1);
  b1 := Ord(c1);

  if b1 and 128 = 0 then
    Result := WideChar(b1)
  else if b1 and 32 = 0 then
  begin
    FTargerStream.Read(c2, 1);
    b2 := Ord(c2);
    b2 := b2 xor 128;
    b1 := b1 xor (128 + 64);
    Value := b2 + b1 shl 6;
    Result := WideChar(Value);

  end
  else if b1 and 16 = 0 then
  begin
    FTargerStream.Read(c2, 1);

    b2 := Ord(c2);
    FTargerStream.Read(c3, 1);

    b3 := Ord(c3);
    b3 := b3 xor 128;
    b2 := b2 xor 128;
    b1 := b1 xor (128 + 64 + 32);
    Value := b3 + b2 shl 6 + b1 shl 12;
    Result := WideChar(Value);

  end
  else if b1 and 8 = 0 then
  begin
    FTargerStream.Read(c2, 1);

    b2 := Ord(c2);
    FTargerStream.Read(c3, 1);

    b3 := Ord(c3);
    FTargerStream.Read(c4, 1);

    b4 := Ord(c4);
    b4 := b4 xor 128;
    b3 := b3 xor 128;
    b2 := b2 xor 128;
    b1 := b1 xor (128 + 64 + 32 + 16);
    Value := b4 + b3 shl 6 + b2 shl 12 + (b1 shl 18);
    Result := WideChar(Value);

  end
  else
    Result := WideChar(' ');

end;

function TMyTextStream.ReadLnWideString: WideString;
var
  Ch: WideChar;
begin
  Result := '';

  while Position < Size do
  begin
    Ch := ReadWideChar;
    if (Ch = #10) then
      Break;
    Result := Result + Ch;

  end;

  if Result = '' then
    Exit;

  if Result[Length(Result)] = #13 then
    WideStrDelete(Result, Length(Result), 1);

end;

function TMyTextStream.ReadWideString: WideString;
var
  Ch: WideChar;
begin
  Result := '';

  while Position < Size do
  begin
    Ch := ReadWideChar;
    if Ch in [#10, ' '] then
      Break;
    Result := Result + Ch;

  end;

  if Result = '' then
    Exit;

  if Result[Length(Result)] = #13 then
    WideStrDelete(Result, Length(Result), 1);

end;

procedure TMyTextStream.WriteStr(const S: AnsiString);
begin
  FTargerStream.Write(Pointer(@S[1])^, Length(S));

end;

constructor TMyTextStream.Create(AnStream: TStream; DeleteInputStream: Boolean);
begin
  inherited Create;

  FTargerStream := AnStream;
  FDeleteInputStream := DeleteInputStream;
  ;

end;

destructor TMyTextStream.Destroy;
begin
  if FDeleteInputStream then
    FTargerStream.Free;

  inherited Destroy;

end;

procedure TMyTextStream.WriteChar(Ch: Char);
begin
  FTargerStream.Write(Ch, 1);

end;

procedure TMyTextStream.WriteLine(const S: AnsiString);
begin
  if Length(S) <> 0 then
    FTargerStream.WriteBuffer(S[1], Length(S));

  (*$ifdef LINUX*)
  WriteChar(#10);

  (*$else*)
  WriteChar(#13);
  WriteChar(#10);
  (*$endif*)

end;

{ TMyBinStream }

function TMyBinStream.GetEndOfStream: Boolean;
begin
  Result := FTargerStream.Position = FTargerStream.Size;

end;

function TMyBinStream.ReadCh: Char;
begin
  FTargerStream.Read(Result, 1);

end;

function TMyBinStream.ReadInt: Integer;
begin
  FTargerStream.ReadBuffer(Result, 4);

end;

function TMyBinStream.ReadInt32: Int32;
begin
  FTargerStream.ReadBuffer(Result, 4);

end;

function TMyBinStream.ReadInt64: Int64;
begin
  FTargerStream.ReadBuffer(Result, 8);

end;

function TMyBinStream.ReadUInt32: UInt32;
begin
  FTargerStream.ReadBuffer(Result, 4);

end;

function TMyBinStream.ReadUInt64: UInt64;
begin
  FTargerStream.ReadBuffer(Result, 8);

end;

function TMyBinStream.ReadByte: Byte;
begin
  FTargerStream.Read(Result, 1);

end;

function TMyBinStream.ReadStr: AnsiString;
var
  Len: Integer;
begin
  Len := ReadUInt32;
  SetLength(Result, Len);
  FTargerStream.Read(Result[1], Len);

end;

procedure TMyBinStream.WriteChar(const Ch: Char);
begin
  FTargerStream.Write(Ch, 1);

end;

procedure TMyBinStream.WriteByte(const b: Byte);
begin
  FTargerStream.Write(b, 1);

end;

procedure TMyBinStream.WriteInt(const n: Integer);
begin
  FTargerStream.Write(n, 4);

end;

procedure TMyBinStream.WriteInt32(const n: Int32);
begin
  FTargerStream.Write(n, 4);

end;

procedure TMyBinStream.WriteInt64(const n: Int64);
begin
  FTargerStream.Write(n, 8);

end;

procedure TMyBinStream.WriteUInt32(const n: uInt32);
begin
  FTargerStream.Write(n, 4);

end;

procedure TMyBinStream.WriteUInt64(const n: uInt64);
begin
  FTargerStream.Write(n, 8);

end;

procedure TMyBinStream.WriteStr(const S: AnsiString);
begin
  WriteUInt32(Length(S));
  FTargerStream.Write(S[1], Length(S));

end;

constructor TMyBinStream.Create(AnStream: TStream; DeleteInputStream: Boolean);
begin
  inherited Create;

  FTargerStream := AnStream;
  FDeleteInputStream := DeleteInputStream;

end;

destructor TMyBinStream.Destroy;
begin
  if FDeleteInputStream then
    FTargerStream.Free;

  inherited Destroy;
end;

end.
