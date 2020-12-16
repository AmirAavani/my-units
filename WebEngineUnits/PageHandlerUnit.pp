unit PageHandlerUnit;

{$mode objfpc}{$H+}

interface

uses
  HttpServerThreadUnit, Classes, SysUtils;

type

  { THTMLBasePageHandler }

  THTMLBasePageHandler = class(TBasePageHandler)
  public
    constructor Create(aName: AnsiString; aServingPath: AnsiString);

    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; override;
    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; override;
  end;

  { TXMLBasePageHandler }

  TXMLBasePageHandler = class(TBasePageHandler)
  public
    constructor Create(aName: AnsiString; aServingPath: AnsiString);

    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; override;
    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; override;
  end;

implementation
uses
  httpprotocol;

{ TXMLBasePageHandler }

constructor TXMLBasePageHandler.Create(aName: AnsiString;
  aServingPath: AnsiString);
begin
  inherited Create(aName, aServingPath);

end;

function TXMLBasePageHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  TheResponse.OriginalResponse.SetHeader(hhContentType, 'text/xml; charset=utf-8');

  Result := True;

end;

function TXMLBasePageHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  Result := ARequest.PathInfo = Self.ServingPath;

end;

{ THTMLBasePageHandler }

constructor THTMLBasePageHandler.Create(aName: AnsiString;
  aServingPath: AnsiString);
begin
  inherited Create(aName, aServingPath);

end;

function THTMLBasePageHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  TheResponse.OriginalResponse.SetHeader(hhContentType, 'text/html; charset=utf-8');

  Result := True;
end;

function THTMLBasePageHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  Result := ARequest.PathInfo = Self.ServingPath;

end;

end.

