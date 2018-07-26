unit HTTPClientUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type

  { THTTPClient }

  THTTPClient = class(TFPHTTPClient)
  public
    function Get(const url: AnsiString; Params: TStringList): AnsiString;

    constructor Create;
  private

  end;

implementation
uses
  zstream;

function ExtractGZipData(const Data: AnsiString): AnsiString;
var
  TmpFileName: AnsiString;
  FileStream: TFileStream;
  Size: Integer;
  GZFileStream: TGZFileStream;

begin
  TmpFileName := GetTempFileName(GetTempDir, 'GzipFile') + '.gz';
  FileStream := TFileStream.Create(TmpFileName, fmCreate);
  FileStream.Write(Data[1], Length(Data));
  FileStream.Free;

  GZFileStream := TGZFileStream.create(TmpFileName, gzopenread);
  SetLength(Result, 10 * Length(Data));

  Size := GZFileStream.read(Result[1], Length(Result));
  GZFileStream.Free;

  DeleteFile(TmpFileName);

  SetLength(Result, Size);
end;

{ THTTPClient }

function THTTPClient.Get(const url: AnsiString; Params: TStringList
  ): AnsiString;
var
  S: AnsiString;
  isGzip: Boolean;
  StrStream: TStringStream;
  GZipStream: Tdecompressionstream;

begin
  IsGZip := False;
  try
    AddHeader('Accept-Encoding', 'Identity');
    Result := inherited Get(Url);

    for s in Self.ResponseHeaders do
    begin
      if Copy(S, 1, Length('Content-Encoding: ')) = 'Content-Encoding: ' then
        if Copy(S, Length('Content-Encoding: ') + 1, Length(S)) = 'gzip' then
          isGzip := True;
    end;

    if isGzip then
      Result := ExtractGZipData(Result);

  except
    on e: Exception do
    begin
      WriteLn('e:', e.Message);
      Exit('');
    end;
  end;
end;

constructor THTTPClient.Create;
begin
  inherited Create(nil);

  Self.AllowRedirect := True;
  Self.MaxRedirects := 10;
  Self.AddHeader('User-Agent', 'KhabarchinBit (compatible; Mozilla/5.0; fpweb)');
  //Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  // Client.AddHeader('User-Agent', 'Mozilla/5.0 (Android 4.4; Mobile; rv:41.0) Gecko/41.0 Firefox/41.0');
  // Client.AddHeader('User-Agent', ' Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/60.0');
  Self.IOTimeout := 20000;
end;

initialization

finalization

end.

