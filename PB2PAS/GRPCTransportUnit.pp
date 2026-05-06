unit GRPCTransportUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GRPCClientConfigUnit, Math;

type
  TByteArray = array of Byte;

  IGRPCTransport = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    
    function SendUnary(
      const MethodPath: string;
      const RequestData: TByteArray;
      out ResponseData: TByteArray;
      out StatusCode: Integer;
      out StatusMessage: string
    ): Boolean;
  end;

  THTTPGRPCTransport = class(TInterfacedObject, IGRPCTransport)
  private
    FConfig: TGRPCClientConfig;
    
    function BuildURL(const MethodPath: string): string;
    function FrameMessage(const Data: TByteArray): TByteArray;
    function UnframeMessage(const FramedData: TByteArray): TByteArray;
    
  public
    constructor Create(AConfig: TGRPCClientConfig);
    destructor Destroy; override;
    
    function SendUnary(
      const MethodPath: string;
      const RequestData: TByteArray;
      out ResponseData: TByteArray;
      out StatusCode: Integer;
      out StatusMessage: string
    ): Boolean;
  end;

implementation

uses
  fphttpclient, httpdefs;

const
  GRPC_STATUS_OK = 0;
  GRPC_CONTENT_TYPE = 'application/grpc+proto';

constructor THTTPGRPCTransport.Create(AConfig: TGRPCClientConfig);
begin
  inherited Create;
  FConfig := AConfig;
end;

destructor THTTPGRPCTransport.Destroy;
begin
  inherited Destroy;
end;

function THTTPGRPCTransport.BuildURL(const MethodPath: string): string;
var
  Protocol: string;
begin
  if FConfig.UseTLS then
    Protocol := 'https'
  else
    Protocol := 'http';
    
  Result := Format('%s://%s:%d%s', [
    Protocol,
    FConfig.Host,
    FConfig.Port,
    MethodPath
  ]);
end;

function THTTPGRPCTransport.FrameMessage(const Data: TByteArray): TByteArray;
var
  Len: UInt32;
  i: Integer;
begin
  Len := Length(Data);
  SetLength(Result, 5 + Len);
  
  Result[0] := 0;
  Result[1] := Byte((Len shr 24) and $FF);
  Result[2] := Byte((Len shr 16) and $FF);
  Result[3] := Byte((Len shr 8) and $FF);
  Result[4] := Byte(Len and $FF);
  
  for i := 0 to Integer(Len) - 1 do
    Result[5 + i] := Data[i];
end;

function THTTPGRPCTransport.UnframeMessage(const FramedData: TByteArray): TByteArray;
var
  MessageLen: UInt32;
  i: Integer;
begin
  Result := nil;
  
  if Length(FramedData) < 5 then
    Exit;
    
  MessageLen := (UInt32(FramedData[1]) shl 24) or
                (UInt32(FramedData[2]) shl 16) or
                (UInt32(FramedData[3]) shl 8) or
                UInt32(FramedData[4]);
  
  if Length(FramedData) < 5 + Integer(MessageLen) then
    Exit;
    
  SetLength(Result, MessageLen);
  for i := 0 to Integer(MessageLen) - 1 do
    Result[i] := FramedData[5 + i];
end;

function THTTPGRPCTransport.SendUnary(
  const MethodPath: string;
  const RequestData: TByteArray;
  out ResponseData: TByteArray;
  out StatusCode: Integer;
  out StatusMessage: string
): Boolean;
var
  HTTPClient: TFPHTTPClient;
  RequestStream: TMemoryStream;
  ResponseStream: TMemoryStream;
  URL: string;
  FramedRequest: TByteArray;
  FramedResponse: TByteArray;
  i: Integer;
  GRPCStatus: string;
begin
  Result := False;
  StatusCode := -1;
  StatusMessage := '';
  ResponseData := nil;

  HTTPClient := TFPHTTPClient.Create(nil);
  RequestStream := nil;
  ResponseStream := nil;
  
  try
    URL := BuildURL(MethodPath);
    
    // Add headers directly
    HTTPClient.AddHeader('Content-Type', GRPC_CONTENT_TYPE);
    HTTPClient.AddHeader('TE', 'trailers');
    HTTPClient.AddHeader('grpc-accept-encoding', 'identity');
    
    // Add auth token if present
    if FConfig.AuthToken <> '' then
      HTTPClient.AddHeader('Authorization', FConfig.AuthToken);
    
    // Add custom headers
    for i := 0 to FConfig.CustomHeaders.Count - 1 do
      HTTPClient.AddHeader(FConfig.CustomHeaders.Names[i], 
                          FConfig.CustomHeaders.ValueFromIndex[i]);
    
    // Frame the request
    FramedRequest := FrameMessage(RequestData);
    
    RequestStream := TMemoryStream.Create;
    RequestStream.Write(FramedRequest[0], Length(FramedRequest));
    RequestStream.Position := 0;
    
    ResponseStream := TMemoryStream.Create;
    
    try
      HTTPClient.RequestBody := RequestStream;
      HTTPClient.HTTPMethod('POST', URL, ResponseStream, [200, 400, 404, 500]);
      
      if HTTPClient.ResponseStatusCode <> 200 then
      begin
        StatusCode := HTTPClient.ResponseStatusCode;
        ResponseStream.Position := 0;
        if ResponseStream.Size > 0 then
        begin
          SetLength(StatusMessage, ResponseStream.Size);
          ResponseStream.Read(StatusMessage[1], ResponseStream.Size);
        end else begin
          StatusMessage := Format('HTTP error: %d', [HTTPClient.ResponseStatusCode]);
        end;
        Exit;
      end;
      
      GRPCStatus := HTTPClient.GetHeader(HTTPClient.ResponseHeaders, 'grpc-status');
      if GRPCStatus = '' then
        GRPCStatus := '0';
        
      StatusCode := StrToIntDef(GRPCStatus, -1);
      StatusMessage := HTTPClient.GetHeader(HTTPClient.ResponseHeaders, 'grpc-message');
      
      if StatusCode <> GRPC_STATUS_OK then
      begin
        if StatusMessage = '' then
          StatusMessage := Format('gRPC error: status code %d', [StatusCode]);
        Exit;
      end;
      
      ResponseStream.Position := 0;
      SetLength(FramedResponse, ResponseStream.Size);
      if ResponseStream.Size > 0 then
        ResponseStream.Read(FramedResponse[0], ResponseStream.Size);
      
      ResponseData := UnframeMessage(FramedResponse);
      
      Result := (ResponseData <> nil) and (Length(ResponseData) > 0);
      
    except
      on E: Exception do
      begin
        StatusCode := -1;
        StatusMessage := E.Message;
        Exit;
      end;
    end;
    
  finally
    RequestStream.Free;
    ResponseStream.Free;
    HTTPClient.Free;
  end;
end;

end.
