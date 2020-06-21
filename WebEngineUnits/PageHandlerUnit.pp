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
  end;

  { TStaticFileHandler }

  TStaticFileHandler = class(THTMLBasePageHandler)
  private
    Content: AnsiString;

  public
    constructor Create(aName, aServingPath, FilePath: AnsiString);

    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; override;
    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse: THTTPServerResponse): Boolean; override;
  end;

implementation
uses
  StreamUnit, httpprotocol;

{ TStaticFileHandler }

constructor TStaticFileHandler.Create(aName, aServingPath, FilePath: AnsiString
  );
begin
  inherited Create(aName, aServingPath);

  Content := ReadFile(FilePath);
end;

function TStaticFileHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  Result := ARequest.PathInfo = ServingPath;
end;

function TStaticFileHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  Result := inherited Execute(Sender, TheRequest, TheResponse);

  TheResponse.WriteLn(Content);

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

end.

