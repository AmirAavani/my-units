unit RPCClientUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, fphttpclient;

type

  { TRPCClient }

  generic TRPCClient<TRequestMessage, TResponseMessage> = class(TFPHTTPClient)
  public
    constructor Create(ServHost: AnsiString; ServerPort: Integer);
    destructor Destroy; override;

    function Call(const Method: AnsiString; const ARequest: TRequestMessage; AResponse: TResponseMessage): Boolean; virtual;

  protected
  private
    HostName: AnsiString;
    Port: Integer;

  end;

implementation
uses
  Math;

{ TRPCClient }

constructor TRPCClient.Create(ServHost: AnsiString; ServerPort: Integer);
begin
  inherited Create(nil);

  HostName := ServHost;
  Port := ServerPort;

end;

destructor TRPCClient.Destroy;
begin

  inherited Destroy;
end;

function TRPCClient.Call(const Method: AnsiString;
  const ARequest: TRequestMessage; AResponse: TResponseMessage): Boolean;
var
  tmp: TRequestMessage;
  ParamStr: AnsiString;
  Response: AnsiString;
  i: Integer;

begin
  WriteLn('ARequest:"', ARequest.ToString, '"');
  ARequest.SaveToString(ParamStr);
  ParamStr := Format('param=%s', [ParamStr]);
  tmp := TRequestMessage.Create;
  if not tmp.LoadFromString(ParamStr) then
  begin
    WriteLn('Failed');
    Exit;

  end;
  WriteLn(Tmp.ToString);

  WriteLn('ParamStr:', ParamStr);
  WriteLn('Len(ParamStr):', Length(ParamStr));

  Response := FormPost(Format('http://%s:%d/%s',
     [HostName, Port, Method]), ParamStr);
  if not AResponse.LoadFromString(Response) then
    WriteLn('Loading Failed');
  AResponse := TResponseMessage.Create;
end;

end.

