unit DefaultPageHandlerUnit;

{$mode objfpc}{$H+}

interface

uses
  HttpServerThreadUnit, PageHandlerUnit, Classes, SysUtils;

type

  { TPageNotFoundHandler }

  TPageNotFoundHandler = class(THTMLBasePageHandler)
  private
  public
    constructor Create;

    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; override;
    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; override;
  end;

implementation
uses
   httpprotocol;


{ TPageNotFoundHandler }

constructor TPageNotFoundHandler.Create;
begin
  inherited Create('PageNotFound', '');

end;

function TPageNotFoundHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  Result := True;
end;

function TPageNotFoundHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  Result := inherited Execute(Sender, TheRequest, TheResponse);

  TheResponse.WriteLn('Do not know how to help you here!');


end;

end.

