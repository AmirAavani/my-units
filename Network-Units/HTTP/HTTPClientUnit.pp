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
  private
    class procedure InitClient;

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
  Client.AllowRedirect:= True;
  Client.IOTimeout := 20000;
end;

initialization
  THTTPClient.InitClient;

finalization
  THTTPClient.Client.Free;

end.

