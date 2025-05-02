unit RPCClientUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, fphttpclient;

type
  {
   TRPCClient is a temporary solution before I implement a compatible GRPC client.
  }

  { TRPCClient }

  TRPCClient = class(TObject)
  protected
    FURL: AnsiString;
    HttpClient: TFPHTTPClient;

  public
    constructor Create(const Host: AnsiString; const Path: AnsiString; Port: UInt32);
    destructor Destroy; override;

    function Call(const Request: TBaseMessage; Response: TBaseMessage): Boolean;

  end;

implementation

{ TRPCClient }

constructor TRPCClient.Create(const Host: AnsiString; const Path: AnsiString;
  Port: UInt32);
begin
  inherited Create;
  FURL := Format('http://%s:%d/%s', [Host, Port, Path]);
  WriteLn(Format('URL: %s', [FURL]));

  HttpClient := TFPHTTPClient.Create(nil);

end;

destructor TRPCClient.Destroy;
begin
  HttpClient.Free;

  inherited Destroy;
end;

function TRPCClient.Call(const Request: TBaseMessage; Response: TBaseMessage
  ): Boolean;
var
  ReqBytes: TBytesStream;
  ResBytes: TBytesStream;

begin
  ReqBytes := TBytesStream.Create;
  ResBytes := TBytesStream.Create;

  Request.SaveToStream(ReqBytes);
  ReqBytes.Free;

  HttpClient.StreamFormPost(FURL, nil, 'Req', 'Req', ReqBytes, ResBytes);
  ResBytes.Position:= 0;
  Result := Response.LoadFromStream(ResBytes);
  ResBytes.Free;

end;

end.

