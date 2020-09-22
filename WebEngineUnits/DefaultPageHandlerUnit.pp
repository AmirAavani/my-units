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

  { TStaticFileHandler }

  TStaticFileHandler = class(TBasePageHandler)
  private
    Content: AnsiString;
    FMIMEType: AnsiString;

  public
    constructor Create(MIMEType, aName, aServingPath, FilePath: AnsiString);

    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse: THTTPServerResponse): Boolean; override;
  end;


  { TStaticHTMLFileHandler }

  TStaticHTMLFileHandler = class(TStaticFileHandler)
  public
    constructor Create(aName, aServingPath, FilePath: AnsiString);

  end;

implementation
uses
   StreamUnit, httpprotocol;

{ TStaticFileHandler }

constructor TStaticFileHandler.Create(MIMEType, aName, aServingPath,
  FilePath: AnsiString);
begin
  inherited Create(aName, aServingPath);

  Content := ReadFile(FilePath);
  FMIMEType := MIMEType;

end;

function TStaticFileHandler.Execute(Sender: THTTPServerThread;
  TheRequest: THTTPServerRequest; TheResponse: THTTPServerResponse): Boolean;
begin
  TheResponse.OriginalResponse.SetHeader(hhContentType, FMIMEType + '; charset=utf-8');

  TheResponse.WriteLn(Content);
  Result := True;

end;

{ TStaticHTMLFileHandler }

constructor TStaticHTMLFileHandler.Create(aName, aServingPath, FilePath: AnsiString
  );
begin
  inherited Create('text/html', aName, aServingPath, FilePath);

end;

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

