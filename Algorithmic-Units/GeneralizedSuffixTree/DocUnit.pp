unit DocUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, StreamUnit;

type
  { TBaseDoc }

  TBaseDoc = class(TObject)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetCharAt(Index: Integer): UInt16; virtual; abstract;

  public
    property CharAt[Index: Integer]: UInt16 read GetCharAt;
    property Count: Integer read GetCount;
    constructor Create;

    function Find(SIndex, EIndex: Integer; Token: Integer): Boolean; virtual;
    function SubStr(SIndex, EIndex: Integer): AnsiString; virtual;

    procedure SaveToStream(Stream: TMyBinStream); virtual; abstract;
    class function LoadFromStream(Stream: TMyBinStream): TBaseDoc; virtual;
  end;

  { TStringDoc }

  TStringDoc = class(TBaseDoc)
  protected
    FStr: AnsiString;

    function GetCount: Integer; override;
    function GetCharAt(Index: Integer): UInt16; override;

  public
    constructor Create(constref Str: AnsiString);
    function SubStr(SIndex, EIndex: Integer): AnsiString; override;

    procedure SaveToStream(Stream: TMyBinStream); override;
    class function LoadFromStream(Stream: TMyBinStream): TBaseDoc; override;

  end;

  { TRefDoc }

  TRefDoc = class(TBaseDoc)
  protected
    BaseDoc: TBaseDoc;
    SIndex, FIndex: Integer;
    ExtraToken: UInt32;
    HasExtraToken: Boolean;
  public
    constructor Create(RefDoc: TBaseDoc; _SIndex, _FIndex: Integer);
    constructor CreateWithExtraToken(RefDoc: TBaseDoc; _SIndex, _FIndex: Integer; EToken: UInt32);

    function GetCount: Integer; override;
    function GetCharAt(Index: Integer): UInt16; override;
    function SubStr(s, e: Integer): AnsiString; override;

    procedure SaveToStream(Stream: TMyBinStream); override;
    class function LoadFromStream(Stream: TMyBinStream): TBaseDoc; override;

  end;

  TBaseDocs = specialize TObjectCollection<TBaseDoc>;

type
  TDocMode = (dmNone = 0, dmStringDoc = 1, dmRefDoc = 2, dmSuffixTreeDoc = 3);


implementation

uses
  StringUnit, ALoggerUnit, SuffixTreeDocUnit;

{ TRefDoc }

constructor TRefDoc.Create(RefDoc: TBaseDoc; _SIndex, _FIndex: Integer);
begin
  inherited Create;

  BaseDoc := RefDoc;
  SIndex := _SIndex;
  FIndex := _FIndex;
  HasExtraToken := False;

end;

constructor TRefDoc.CreateWithExtraToken(RefDoc: TBaseDoc; _SIndex,
  _FIndex: Integer; EToken: UInt32);
begin
  inherited Create;

  BaseDoc := RefDoc;
  SIndex := _SIndex;
  FIndex := _FIndex;

  ExtraToken:= EToken;
  HasExtraToken := True;
end;

function TRefDoc.GetCount: Integer;
begin
  Result := FIndex - SIndex + 1;
end;

function TRefDoc.GetCharAt(Index: Integer): UInt16;
begin
  if Index = Count - 1 then
    Exit(0);
  Result := BaseDoc.CharAt[Index + SIndex];

end;

function TRefDoc.SubStr(s, e: Integer): AnsiString;
begin
  if e + SIndex = BaseDoc.Count + 1 then
  begin
    Result := BaseDoc.SubStr(s + SIndex, e + SIndex) + '(EOF)';
    Exit;

  end;

  Result := BaseDoc.SubStr(s + SIndex, e + SIndex);
end;

procedure TRefDoc.SaveToStream(Stream: TMyBinStream);
begin

end;

class function TRefDoc.LoadFromStream(Stream: TMyBinStream): TBaseDoc;
begin
  Result:=inherited LoadFromStream(Stream);
end;

{ TBaseDoc }

constructor TBaseDoc.Create;
begin
  inherited Create;

end;

function TBaseDoc.Find(SIndex, EIndex: Integer; Token: Integer): Boolean;
var
  i: Integer;

begin
  Result := False;

  for i := SIndex to EIndex do
    if Self.CharAt[i] = Token then
      Exit(True);

end;

function TBaseDoc.SubStr(SIndex, EIndex: Integer): AnsiString;
var
  i: Integer;
  StrList: TStringList;

begin
  Result := '';

  StrList := TStringList.Create;

  for i := SIndex to EIndex do
    StrList.Add(IntToStr(Self.CharAt[i]));

  Result := '(' + JoinStrings(StrList, '),(') + ')';

  StrList.Free;
end;

class function TBaseDoc.LoadFromStream(Stream: TMyBinStream): TBaseDoc;
var
  b: Byte;
  Str: AnsiString;

begin
  b := Stream.ReadByte;
  Result := nil;

  case b of
  Ord(dmStringDoc):
  begin
    Result := TStringDoc.LoadFromStream(Stream);

  end;
  Ord(dmSuffixTreeDoc):
  begin
    Result := TSuffixTreeDoc.LoadFromStream(Stream);

  end
  else
    ALoggerUnit.FmtFatalLn('Invalid mode: %d', [b]);
  end;
end;

{ TStringDoc }

function TStringDoc.GetCount: Integer;
begin
  Result := Length(FStr);

end;

function TStringDoc.GetCharAt(Index: Integer): UInt16;
begin
  Result := Ord(FStr[Index + 1]);

end;

constructor TStringDoc.Create(constref Str: AnsiString);
begin
  inherited Create;

  FStr := Str;

end;

function TStringDoc.SubStr(SIndex, EIndex: Integer): AnsiString;
begin
  Result := Copy(FStr, SIndex + 1, EIndex - SIndex + 1);

end;

procedure TStringDoc.SaveToStream(Stream: TMyBinStream);
begin
  Stream.WriteByte(Ord(dmStringDoc));
  Stream.WriteStr(FStr);

end;

class function TStringDoc.LoadFromStream(Stream: TMyBinStream): TBaseDoc;
var
  Str: AnsiString;

begin
  Str := Stream.ReadStr;
  Result := TStringDoc.Create(Str);

end;

end.

