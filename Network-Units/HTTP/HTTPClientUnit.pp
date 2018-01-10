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

    class function GetClient: THTTPClient; static;
  private
    class procedure InitClient;

  end;

implementation

{ THTTPClient }

function THTTPClient.Get(const url: AnsiString; Params: TStringList
  ): AnsiString;
begin
  Result := inherited Get(Url);
end;

class function THTTPClient.GetClient: THTTPClient;
begin
  Result := Client;
end;

class procedure THTTPClient.InitClient;
begin
  Client := THTTPClient.Create(nil);

end;

initialization
  THTTPClient.InitClient;
end.

