unit GRPCClientUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, ProtoStreamUnit, 
  GRPCClientConfigUnit, GRPCTransportUnit;

type
  // Use dynamic array for gRPC transport (not the TSimpleTypeList from ProtoHelperUnit)
  TByteArray = array of Byte;
  { TGRPCClient - Base class for all generated gRPC clients }
  TGRPCClient = class
  private
    FConfig: TGRPCClientConfig;
    FTransport: IGRPCTransport;
    FServiceName: string;
    FOwnsConfig: Boolean;
    FLastStatusCode: Integer;
    FLastStatusMessage: string;
    FRaiseOnError: Boolean;
    
  protected
    // Core method that generated client code calls
    function CallUnary(
      const AMethodName: string;
      ARequest: TBaseMessage;
      AResponse: TBaseMessage
    ): Boolean;
    
    // Helper methods
    function GetMethodPath(const AMethodName: string): string;
    function SerializeRequest(ARequest: TBaseMessage): TByteArray;
    function DeserializeResponse(const Data: TByteArray; AResponse: TBaseMessage): Boolean;
    
  public
    // Constructor with full config
    constructor Create(AConfig: TGRPCClientConfig; const AServiceName: string); overload;
    
    // Simple constructor for basic usage
    constructor CreateSimple(const AHost: string; APort: Integer; 
      const AServiceName: string); overload;
    
    destructor Destroy; override;
    
    // Properties
    property ServiceName: string read FServiceName;
    property Config: TGRPCClientConfig read FConfig;
    property LastStatusCode: Integer read FLastStatusCode;
    property LastStatusMessage: string read FLastStatusMessage;
    property RaiseOnError: Boolean read FRaiseOnError write FRaiseOnError;
  end;
  
  { EGRPCError - Exception for gRPC errors }
  EGRPCError = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(AStatusCode: Integer; const AMessage: string);
    property StatusCode: Integer read FStatusCode;
  end;

implementation

{ EGRPCError }

constructor EGRPCError.Create(AStatusCode: Integer; const AMessage: string);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
end;

{ TGRPCClient }

constructor TGRPCClient.Create(AConfig: TGRPCClientConfig; const AServiceName: string);
begin
  inherited Create;
  FConfig := AConfig;
  FOwnsConfig := False;
  FServiceName := AServiceName;
  FTransport := THTTPGRPCTransport.Create(FConfig);
  FLastStatusCode := 0;
  FLastStatusMessage := '';
  FRaiseOnError := False;
end;

constructor TGRPCClient.CreateSimple(const AHost: string; APort: Integer; 
  const AServiceName: string);
begin
  inherited Create;
  FConfig := TGRPCClientConfig.Create;
  FConfig.Host := AHost;
  FConfig.Port := APort;
  FOwnsConfig := True;
  FServiceName := AServiceName;
  FTransport := THTTPGRPCTransport.Create(FConfig);
  FLastStatusCode := 0;
  FLastStatusMessage := '';
  FRaiseOnError := False;
end;

destructor TGRPCClient.Destroy;
begin
  FTransport := nil; // Interface will be freed automatically
  if FOwnsConfig then
    FConfig.Free;
  inherited Destroy;
end;

function TGRPCClient.GetMethodPath(const AMethodName: string): string;
begin
  // gRPC method path format: /{package.ServiceName}/{MethodName}
  Result := '/' + FServiceName + '/' + AMethodName;
end;

function TGRPCClient.SerializeRequest(ARequest: TBaseMessage): TByteArray;
var
  Stream: TMemoryStream;
begin
  Result := nil;
  if ARequest = nil then
    Exit;
    
  Stream := TMemoryStream.Create;
  try
    // TBaseMessage.SaveToStream(TStream) internally creates TProtoStreamWriter
    ARequest.SaveToStream(Stream);
    Stream.Position := 0;
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.Read(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TGRPCClient.DeserializeResponse(const Data: TByteArray; 
  AResponse: TBaseMessage): Boolean;
var
  Stream: TMemoryStream;
begin
  Result := False;
  if (AResponse = nil) or (Length(Data) = 0) then
    Exit;
    
  Stream := TMemoryStream.Create;
  try
    Stream.Write(Data[0], Length(Data));
    Stream.Position := 0;
    // TBaseMessage.LoadFromStream(TStream) internally creates TProtoStreamReader
    Result := AResponse.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TGRPCClient.CallUnary(
  const AMethodName: string;
  ARequest: TBaseMessage;
  AResponse: TBaseMessage
): Boolean;
var
  MethodPath: string;
  RequestData: TByteArray;
  ResponseData: TByteArray;
  StatusCode: Integer;
  StatusMessage: string;
begin
  Result := False;
  FLastStatusCode := -1;
  FLastStatusMessage := '';
  
  try
    // Get the full method path
    MethodPath := GetMethodPath(AMethodName);
    
    // Serialize the request
    RequestData := SerializeRequest(ARequest);
    if RequestData = nil then
    begin
      FLastStatusMessage := 'Failed to serialize request';
      if FRaiseOnError then
        raise EGRPCError.Create(-1, FLastStatusMessage);
      Exit;
    end;
    
    // Send the request via transport
    if not FTransport.SendUnary(MethodPath, RequestData, ResponseData, 
                                StatusCode, StatusMessage) then
    begin
      FLastStatusCode := StatusCode;
      FLastStatusMessage := StatusMessage;
      if FRaiseOnError then
        raise EGRPCError.Create(StatusCode, StatusMessage);
      Exit;
    end;
    
    // Deserialize the response
    if not DeserializeResponse(ResponseData, AResponse) then
    begin
      FLastStatusCode := -1;
      FLastStatusMessage := 'Failed to deserialize response';
      if FRaiseOnError then
        raise EGRPCError.Create(-1, FLastStatusMessage);
      Exit;
    end;
    
    FLastStatusCode := StatusCode;
    FLastStatusMessage := StatusMessage;
    Result := True;
    
  except
    on E: Exception do
    begin
      if not (E is EGRPCError) then
      begin
        FLastStatusCode := -1;
        FLastStatusMessage := E.Message;
        if FRaiseOnError then
          raise;
      end;
    end;
  end;
end;

end.
