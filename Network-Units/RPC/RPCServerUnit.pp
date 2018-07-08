unit RPCServerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, fphttpserver;

type
  { TRPCServer }

  generic TRPCServer<TRequestMessage, TResponseMessage> = class(TFPHttpServer)
  public
    constructor Create(ServPort: Integer);
    destructor Destroy; override;

    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
      override;
    procedure ServeRequest(const FuncName: AnsiString;
        const ARequest: TRequestMessage;
        AResponse: TResponseMessage); virtual; abstract;
  protected

  end;

implementation
uses
  math, fpmimetypes;

{ TRPCServer }

constructor TRPCServer.Create(ServPort: Integer);
begin
  inherited Create(nil);

  Self.Port := ServPort;

end;

destructor TRPCServer.Destroy;
begin
  inherited Destroy;
end;

procedure TRPCServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Request: TRequestMessage;
  Response: TResponseMessage;
  OutputStream: TStringStream;
  Text: AnsiString;
  i: Integer;

begin
  Request := TRequestMessage.Create;
  WriteLn('Len(Content):', Length(ARequest.Content));
  WriteLn('Received Params:', ARequest.Content);
  for i := 1 to Min(Length(ARequest.Content), 10) do
    WriteLn(Ord(ARequest.Content[i]), ':', ARequest.Content[i]);

  if not Request.LoadFromString(ARequest.Content) then
  begin
    WriteLn('Failed');
    Exit;

  end;

  Response := TResponseMessage.Create;

  Response.Status := 'ABC';
  //Self.ServeRequest(ARequest.PathInfo, Request, Response);

  Response.SaveToString(Text);
  OutputStream := TStringStream.Create(Text);

  AResponse.ContentType := 'text/html';
  OutputStream.Position := 0;

  AResponse.ContentStream := OutputStream;

  AResponse.SendContent;
{
  Request.Free;
  Response.Free;
}
end;

end.

