unit SuffixTreeDocUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DocUnit, MetadataUnit, StreamUnit;

const
  EndToken: UInt16 = 0;

type

  { TSuffixTreeDoc }

  TSuffixTreeDoc = class(TBaseDoc)
  private
    Doc: TBaseDoc;
    MetaData: TDocMetaData;
    MetaDataBs: TBytes;

  protected
    function GetCount: Integer; override;
    function GetCharAt(Index: Integer): UInt16; override;

  public
    constructor Create(_Doc: TBaseDoc; _Metadata: TDocMetaData);
    destructor Destroy; override;
    function SubStr(b, e: Integer): AnsiString; override;

    procedure SaveToStream(Stream: TMyBinStream); override;
    class function LoadFromStream(Stream: TMyBinStream): TBaseDoc; override;

  end;


implementation
uses
  Math;

{ TSuffixTreeDoc }

function TSuffixTreeDoc.GetCount: Integer;
begin
  Result := Doc.Count + 1 + (Length(MetaDataBs) + 1) div 2;


end;

function TSuffixTreeDoc.GetCharAt(Index: Integer): UInt16;
var
  i: Integer;

begin
  if Index < Doc.Count then
    Exit(Doc.CharAt[Index]);

  if Index = Doc.Count then
    Exit(EndToken);

  {
   C+1, -> M[0], M[1]
   C+2  -> M[2], M[3]
   ...
  }
  i := Index - Doc.Count - 1;
  Result := Ord(MetaDataBs[2 * i]) shl 8;
  if 2 * i + 1 < Length(MetaDataBs) then
    Result := Result or UInt16(MetaDataBs[2 * i + 1]);

end;

constructor TSuffixTreeDoc.Create(_Doc: TBaseDoc; _Metadata: TDocMetaData);
var
  S: TStream;

begin
  inherited Create;

  Doc := _Doc;
  MetaData := _Metadata;

  S := TBytesStream.Create;
  Metadata.SaveToStream(S);
  S.Position := 0;
  SetLength(MetaDataBs, S.Size);
  S.ReadData(MetaDataBs, S.Size);
  S.Free;

end;

destructor TSuffixTreeDoc.Destroy;
begin
  Doc.Free;
  MetaData.Free;
  SetLength(MetaDataBs, 0);

  inherited Destroy;
end;

function TSuffixTreeDoc.SubStr(b, e: Integer): AnsiString;
var
  i: Integer;

begin
  Result := Doc.SubStr(b, Min(e, Doc.Count - 1));
  if e < Doc.Count then
    Exit;

  Result += '(EOF)';
  if e = Doc.Count then
    Exit;

  Result += '{';
  for i := Doc.Count + 1 to e do
    Result += IntToStr(Self.CharAt[i]) + ',';
  Delete(Result, Length(Result), 1);
  Result += '}';


end;

procedure TSuffixTreeDoc.SaveToStream(Stream: TMyBinStream);
var
  ss: TStringStream;

begin
  Stream.WriteByte(Ord(dmSuffixTreeDoc));
  Doc.SaveToStream(Stream);
  ss := TStringStream.Create;
  MetaData.SaveToStream(ss);
  Stream.WriteStr(ss.DataString);
  ss.Free;


end;

class function TSuffixTreeDoc.LoadFromStream(Stream: TMyBinStream): TBaseDoc;
var
  d: TBaseDoc;
  Str: AnsiString;
  ss: TStringStream;
  md: TDocMetaData;
begin
  d := TBaseDoc.LoadFromStream(Stream);

  Str := Stream.ReadStr;
  ss := TStringStream.Create(Str);
  md := TDocMetaData.Create;
  md.LoadFromStream(ss);

  Result:= TSuffixTreeDoc.Create(d, md);
end;

end.

