unit GRPCServerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, ProtoStreamUnit,
  GRPCServerConfigUnit, GRPCServerTransportUnit,
  fgl, syncobjs;

type
  TByteArray = array of Byte;
  
  { TGRPCServiceHandler - Base class for all generated service stubs }
  TGRPCServiceHandler = class
  public
    // Override this in generated service classes
    function HandleRequest(const Method: AnsiString; 
      ReqData: TByteArray): TByteArray; virtual; abstract;
    
    // Get the service name (override in generated classes)
    function GetServiceName: AnsiString; virtual; abstract;
  end;

  { TGRPCServiceHandlerList }
  TGRPCServiceHandlerList = specialize TFPGMap<AnsiString, TGRPCServiceHandler>;

  { TGRPCServer - Main server class }
  TGRPCServer = class
  private
    FConfig: TGRPCServerConfig;
    FTransport: IGRPCServerTransport;
    FServices: TGRPCServiceHandlerList;
    FRunning: Boolean;
    FOwnsConfig: Boolean;
    FLock: TCriticalSection;
    
    procedure HandleIncomingRequest(const ServiceName, MethodNameStr: AnsiString;
      RequestData: TByteArray; out ResponseData: TByteArray;
      out StatusCode: Integer; out StatusMessage: AnsiString);
    
  public
    // Constructor with config
    constructor Create(AConfig: TGRPCServerConfig); overload;
    
    // Simple constructor
    constructor CreateSimple(APort: Integer); overload;
    
    destructor Destroy; override;
    
    // Register a service handler
    procedure RegisterService(const ServiceName: AnsiString; 
      Handler: TGRPCServiceHandler);
    
    // Unregister a service
    procedure UnregisterService(const ServiceName: AnsiString);
    
    // Server lifecycle
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    
    // Properties
    property Config: TGRPCServerConfig read FConfig;
    property Running: Boolean read FRunning;
  end;
  
  { EGRPCServerError - Exception for server errors }
  EGRPCServerError = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(AStatusCode: Integer; const AMessage: string);
    property StatusCode: Integer read FStatusCode;
  end;

implementation

{ EGRPCServerError }

constructor EGRPCServerError.Create(AStatusCode: Integer; const AMessage: string);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
end;

{ TGRPCServer }

constructor TGRPCServer.Create(AConfig: TGRPCServerConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FOwnsConfig := False;
  FServices := TGRPCServiceHandlerList.Create;
  FServices.Sorted := True;
  FRunning := False;
  FLock := TCriticalSection.Create;
  FTransport := nil;
end;

constructor TGRPCServer.CreateSimple(APort: Integer);
begin
  inherited Create;
  FConfig := TGRPCServerConfig.Create;
  FConfig.Port := APort;
  FOwnsConfig := True;
  FServices := TGRPCServiceHandlerList.Create;
  FServices.Sorted := True;
  FRunning := False;
  FLock := TCriticalSection.Create;
  FTransport := nil;
end;

destructor TGRPCServer.Destroy;
begin
  if FRunning then
    Stop;
    
  FTransport := nil; // Interface will be freed automatically
  FServices.Free;
  FLock.Free;
  
  if FOwnsConfig then
    FConfig.Free;
    
  inherited Destroy;
end;

procedure TGRPCServer.RegisterService(const ServiceName: AnsiString;
  Handler: TGRPCServiceHandler);
begin
  FLock.Enter;
  try
    if FServices.IndexOf(ServiceName) >= 0 then
      raise EGRPCServerError.Create(-1, 
        Format('Service "%s" is already registered', [ServiceName]));
    
    FServices.Add(ServiceName, Handler);
  finally
    FLock.Leave;
  end;
end;

procedure TGRPCServer.UnregisterService(const ServiceName: AnsiString);
var
  Index: Integer;
begin
  FLock.Enter;
  try
    Index := FServices.IndexOf(ServiceName);
    if Index >= 0 then
      FServices.Delete(Index);
  finally
    FLock.Leave;
  end;
end;

procedure TGRPCServer.HandleIncomingRequest(
  const ServiceName, MethodNameStr: AnsiString;
  RequestData: TByteArray; 
  out ResponseData: TByteArray;
  out StatusCode: Integer; 
  out StatusMessage: AnsiString);
var
  Index: Integer;
  Handler: TGRPCServiceHandler;
begin
  ResponseData := nil;
  StatusCode := 12; // UNIMPLEMENTED in gRPC status codes
  StatusMessage := 'Service not found';
  
  FLock.Enter;
  try
    Index := FServices.IndexOf(ServiceName);
    if Index < 0 then
    begin
      StatusCode := 5; // NOT_FOUND
      StatusMessage := Format('Service "%s" not found', [ServiceName]);
      Exit;
    end;
    
    Handler := FServices.Data[Index];
  finally
    FLock.Leave;
  end;
  
  try
    // Call the service handler
    ResponseData := Handler.HandleRequest(MethodNameStr, RequestData);
    
    if ResponseData = nil then
    begin
      StatusCode := 2; // UNKNOWN
      StatusMessage := 'Handler returned nil response';
      Exit;
    end;
    
    StatusCode := 0; // OK
    StatusMessage := 'OK';
    
  except
    on E: Exception do
    begin
      StatusCode := 13; // INTERNAL
      StatusMessage := 'Internal error: ' + E.Message;
      ResponseData := nil;
    end;
  end;
end;

procedure TGRPCServer.Start;
begin
  if FRunning then
    raise EGRPCServerError.Create(-1, 'Server is already running');
  
  if FServices.Count = 0 then
    raise EGRPCServerError.Create(-1, 'No services registered');
  
  // Create transport
  FTransport := THTTPGRPCServerTransport.Create(FConfig, @HandleIncomingRequest, True); // True = use gRPC-Web
  
  // Start listening
  FTransport.Start;
  FRunning := True;
end;

procedure TGRPCServer.Stop;
begin
  if not FRunning then
    Exit;
    
  if FTransport <> nil then
    FTransport.Stop;
    
  FRunning := False;
end;

function TGRPCServer.IsRunning: Boolean;
begin
  Result := FRunning;
end;

end.
