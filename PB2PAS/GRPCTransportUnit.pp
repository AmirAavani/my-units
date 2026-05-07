unit GRPCTransportUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GRPCClientConfigUnit, Math, base64;

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
    FUseGRPCWeb: Boolean; // Use gRPC-Web instead of standard gRPC
    
    function BuildURL(const MethodPath: string): string;
    function FrameMessage(const Data: TByteArray): TByteArray;
    function UnframeMessage(const FramedData: TByteArray): TByteArray;
    function ExtractGRPCWebResponse(const ResponseBody: TByteArray; 
      out StatusCode: Integer; out StatusMessage: string): TByteArray;
    
  public
    constructor Create(AConfig: TGRPCClientConfig; AUseGRPCWeb: Boolean = True);
    destructor Destroy; override;
    
    function SendUnary(
      const MethodPath: string;
      const RequestData: TByteArray;
      out ResponseData: TByteArray;
      out StatusCode: Integer;
      out StatusMessage: string
    ): Boolean;
    
    property UseGRPCWeb: Boolean read FUseGRPCWeb write FUseGRPCWeb;
  end;

implementation

uses
  fphttpclient, httpdefs;

const
  GRPC_STATUS_OK = 0;
  GRPC_CONTENT_TYPE = 'application/grpc+proto';
  GRPC_WEB_CONTENT_TYPE = 'application/grpc-web+proto';
  GRPC_WEB_TEXT_CONTENT_TYPE = 'application/grpc-web-text+proto';
  
  // Frame types for gRPC-Web trailers
  GRPC_WEB_FLAG_TRAILER = $80;

constructor THTTPGRPCTransport.Create(AConfig: TGRPCClientConfig; AUseGRPCWeb: Boolean);
begin
  inherited Create;
  FConfig := AConfig;
  FUseGRPCWeb := AUseGRPCWeb;
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

function THTTPGRPCTransport.ExtractGRPCWebResponse(const ResponseBody: TByteArray;
  out StatusCode: Integer; out StatusMessage: string): TByteArray;
var
  BytePos: Integer;
  MessageLen: UInt32;
  Flags: Byte;
  TrailerData: TByteArray;
  TrailerStr: string;
  TrailerLines: TStringList;
  i: Integer;
  Line: string;
begin
  Result := nil;
  StatusCode := -1;
  StatusMessage := '';
  
  if Length(ResponseBody) < 5 then
    Exit;
  
  BytePos := 0;
  
  // Read first frame (should be the data frame)
  Flags := ResponseBody[BytePos];
  Inc(BytePos);
  
  MessageLen := (UInt32(ResponseBody[BytePos]) shl 24) or
                (UInt32(ResponseBody[BytePos + 1]) shl 16) or
                (UInt32(ResponseBody[BytePos + 2]) shl 8) or
                UInt32(ResponseBody[BytePos + 3]);
  Inc(BytePos, 4);
  
  // Extract message data
  if (Flags and GRPC_WEB_FLAG_TRAILER) = 0 then
  begin
    // This is the data frame
    SetLength(Result, MessageLen);
    if MessageLen > 0 then
      Move(ResponseBody[BytePos], Result[0], MessageLen);
    Inc(BytePos, MessageLen);
  end;
  
  // Read trailers frame if present
  if BytePos < Length(ResponseBody) - 5 then
  begin
    Flags := ResponseBody[BytePos];
    Inc(BytePos);
    
    MessageLen := (UInt32(ResponseBody[BytePos]) shl 24) or
                  (UInt32(ResponseBody[BytePos + 1]) shl 16) or
                  (UInt32(ResponseBody[BytePos + 2]) shl 8) or
                  UInt32(ResponseBody[BytePos + 3]);
    Inc(BytePos, 4);
    
    if (Flags and GRPC_WEB_FLAG_TRAILER) <> 0 then
    begin
      // This is the trailers frame
      SetLength(TrailerData, MessageLen);
      if MessageLen > 0 then
        Move(ResponseBody[BytePos], TrailerData[0], MessageLen);
      
      // Parse trailers (text format: "grpc-status: 0\r\ngrpc-message: OK\r\n")
      SetLength(TrailerStr, MessageLen);
      if MessageLen > 0 then
        Move(TrailerData[0], TrailerStr[1], MessageLen);
      
      TrailerLines := TStringList.Create;
      try
        TrailerLines.Text := TrailerStr;
        
        for i := 0 to TrailerLines.Count - 1 do
        begin
          Line := TrailerLines[i];
          if Pos('grpc-status:', Line) = 1 then
            StatusCode := StrToIntDef(Trim(Copy(Line, 13, 100)), -1)
          else if Pos('grpc-message:', Line) = 1 then
            StatusMessage := Trim(Copy(Line, 14, 1000));
        end;
      finally
        TrailerLines.Free;
      end;
    end;
  end;
  
  // Default status if not found in trailers
  if StatusCode = -1 then
    StatusCode := GRPC_STATUS_OK;
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
  ResponseBody: TByteArray;
  i: Integer;
  ContentType: string;
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
    
    // Choose content type based on mode
    if FUseGRPCWeb then
      ContentType := GRPC_WEB_CONTENT_TYPE
    else
      ContentType := GRPC_CONTENT_TYPE;
    
    // Add headers
    HTTPClient.AddHeader('Content-Type', ContentType);
    HTTPClient.AddHeader('X-User-Agent', 'grpc-web-pascal/1.0');
    HTTPClient.AddHeader('X-Grpc-Web', '1');
    
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
      
      // Read response body
      ResponseStream.Position := 0;
      SetLength(ResponseBody, ResponseStream.Size);
      if ResponseStream.Size > 0 then
        ResponseStream.Read(ResponseBody[0], ResponseStream.Size);
      
      // Extract response data and trailers
      if FUseGRPCWeb then
      begin
        // gRPC-Web: trailers are in the body
        ResponseData := ExtractGRPCWebResponse(ResponseBody, StatusCode, StatusMessage);
      end
      else
      begin
        // Standard gRPC: trailers are in HTTP headers
        ResponseData := UnframeMessage(ResponseBody);
        
        // Get status from headers
        StatusCode := StrToIntDef(
          HTTPClient.GetHeader(HTTPClient.ResponseHeaders, 'grpc-status'), 0);
        StatusMessage := 
          HTTPClient.GetHeader(HTTPClient.ResponseHeaders, 'grpc-message');
      end;
      
      if StatusCode <> GRPC_STATUS_OK then
      begin
        if StatusMessage = '' then
          StatusMessage := Format('gRPC error: status code %d', [StatusCode]);
        Exit;
      end;
      
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
