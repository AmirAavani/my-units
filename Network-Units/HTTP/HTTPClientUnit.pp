unit HTTPClientUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type

  { THTTPClient }

  THTTPClient = class(TFPHTTPClient)
  private
    class var Client: THTTPClient;
  public
    function Get(const url: AnsiString; Params: TStringList): AnsiString;

    class function GetClient: THTTPClient;
    class procedure InitClient;
  private

  end;

implementation

{ THTTPClient }

function THTTPClient.Get(const url: AnsiString; Params: TStringList
  ): AnsiString;

begin
  try
    Result := inherited Get(Url);
  except
    on e: Exception do
    begin
      WriteLn('e:', e.Message);
      Exit('');
    end;
  end;
end;

class function THTTPClient.GetClient: THTTPClient;
begin
  Result := Client;

end;

class procedure THTTPClient.InitClient;
begin
  Client := THTTPClient.Create(nil);
  Client.AllowRedirect := True;
  Client.MaxRedirects := 10;
  Client.AddHeader('User-Agent', 'KhabarchinBit (compatible; Mozilla/5.0; fpweb)');
  Client.IOTimeout := 20000;
end;

initialization

finalization

end.

