unit PageHandlerUnit;

{$mode objfpc}{$H+}

interface

uses
  HttpServerThreadUnit, Classes, SysUtils;

type

  { THTMLBasePageHandler }

  THTMLBasePageHandler = class(TBasePageHandler)
  public
    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; override;
    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; override;
  end;

  { TXMLBasePageHandler }

  TXMLBasePageHandler = class(TBasePageHandler)
  public
    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; override;
  end;

implementation
uses
  httpprotocol, ALoggerUnit;

{ TXMLBasePageHandler }

function TXMLBasePageHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  TheResponse.OriginalResponse.SetHeader(hhContentType, 'text/xml; charset=utf-8');

  Result := True;

end;

{ THTMLBasePageHandler }

function THTMLBasePageHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  TheResponse.OriginalResponse.SetHeader(hhContentType, 'text/html; charset=utf-8');

  Result := True;
end;

function THTMLBasePageHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  Result := 0 <= Self.ServingPaths.IndexOf(ARequest.PathInfo);

end;

end.

