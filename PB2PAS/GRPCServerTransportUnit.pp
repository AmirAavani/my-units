unit GRPCServerTransportUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, GRPCServerConfigUnit, cthreads, Math;

type
  TByteArray = array of Byte;
  
  { TRequestHandler - Callback type for handling requests }
  TRequestHandler = procedure(
    const ServiceName, MethodName: AnsiString;
    RequestData: TByteArray;
    out ResponseData: TByteArray;
    out StatusCode: Integer;
    out StatusMessage: AnsiString) of object;

  { IGRPCServerTransport - Transport interface for server }
  IGRPCServerTransport = interface
    ['{8F3D4E5C-1A2B-3C4D-5E6F-7A8B9C0D1E2F}']
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
  end;

  { THTTPGRPCServerTransport - Simple HTTP/1.1 based transport with gRPC-Web support }
  THTTPGRPCServerTransport = class(TInterfacedObject, IGRPCServerTransport)
  private
    FConfig: TGRPCServerConfig;
    FRequestHandler: TRequestHandler;
    FServerSocket: TSocket;
    FRunning: Boolean;
    FListenerThread: TThread;
    FUseGRPCWeb: Boolean; // Support gRPC-Web for broader compatibility
    
    procedure ListenerThreadProc;
    procedure HandleConnection(ClientSocket: TSocket);
    function ParseHTTPRequest(ClientSocket: TSocket; 
      out Method, Path, ContentType: AnsiString;
      out ContentLength: Integer;
      out BodyData: TByteArray): Boolean;
    function ReadRequestBody(ClientSocket: TSocket; ContentLength: Integer): TByteArray;
    procedure SendHTTPResponse(ClientSocket: TSocket; 
      StatusCode: Integer; const StatusMessage: AnsiString;
      const ResponseData: TByteArray);
    procedure ParsePath(const Path: AnsiString; 
      out ServiceName, MethodNameStr: AnsiString);
    function UnframeMessage(const FramedData: TByteArray): TByteArray;
    
  public
    constructor Create(AConfig: TGRPCServerConfig; 
      ARequestHandler: TRequestHandler; AUseGRPCWeb: Boolean = True);
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
  end;

  { TListenerThread - Thread for accepting connections }
  TListenerThread = class(TThread)
  private
    FTransport: THTTPGRPCServerTransport;
  public
    constructor Create(ATransport: THTTPGRPCServerTransport);
    procedure Execute; override;
  end;

implementation

{ TListenerThread }

constructor TListenerThread.Create(ATransport: THTTPGRPCServerTransport);
begin
  inherited Create(False);
  FTransport := ATransport;
  FreeOnTerminate := False;
end;

procedure TListenerThread.Execute;
begin
  FTransport.ListenerThreadProc;
end;

{ THTTPGRPCServerTransport }

constructor THTTPGRPCServerTransport.Create(AConfig: TGRPCServerConfig;
  ARequestHandler: TRequestHandler; AUseGRPCWeb: Boolean);
begin
  inherited Create;
  FConfig := AConfig;
  FRequestHandler := ARequestHandler;
  FServerSocket := -1;
  FRunning := False;
  FListenerThread := nil;
  FUseGRPCWeb := AUseGRPCWeb;
end;

destructor THTTPGRPCServerTransport.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure THTTPGRPCServerTransport.Start;
var
  Addr: TInetSockAddr;
  OptVal: Integer;
begin
  if FRunning then
    Exit;
  
  // Create socket
  FServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FServerSocket < 0 then
    raise Exception.Create('Failed to create socket');
  
  // Set socket options (SO_REUSEADDR = 1)
  OptVal := 1;
  fpSetSockOpt(FServerSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));
  
  // Bind to address
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(FConfig.Port);
  if FConfig.Host = '0.0.0.0' then
    Addr.sin_addr.s_addr := INADDR_ANY
  else
    Addr.sin_addr := StrToNetAddr(FConfig.Host);
  
  if fpBind(FServerSocket, @Addr, SizeOf(Addr)) < 0 then
  begin
    CloseSocket(FServerSocket);
    raise Exception.CreateFmt('Failed to bind to %s:%d', [FConfig.Host, FConfig.Port]);
  end;
  
  // Listen for connections
  if fpListen(FServerSocket, FConfig.MaxConnections) < 0 then
  begin
    CloseSocket(FServerSocket);
    raise Exception.Create('Failed to listen on socket');
  end;
  
  FRunning := True;
  
  // Start listener thread
  FListenerThread := TListenerThread.Create(Self);
end;

procedure THTTPGRPCServerTransport.Stop;
begin
  if not FRunning then
    Exit;
  
  FRunning := False;
  
  // Stop listener thread
  if FListenerThread <> nil then
  begin
    FListenerThread.Terminate;
    FListenerThread.WaitFor;
    FListenerThread.Free;
    FListenerThread := nil;
  end;
  
  // Close server socket
  if FServerSocket >= 0 then
  begin
    CloseSocket(FServerSocket);
    FServerSocket := -1;
  end;
end;

function THTTPGRPCServerTransport.IsRunning: Boolean;
begin
  Result := FRunning;
end;

procedure THTTPGRPCServerTransport.ListenerThreadProc;
var
  ClientSocket: TSocket;
  ClientAddr: TInetSockAddr;
  AddrLen: TSockLen;
begin
  while not TThread.CurrentThread.CheckTerminated do
  begin
    AddrLen := SizeOf(ClientAddr);
    ClientSocket := fpAccept(FServerSocket, @ClientAddr, @AddrLen);
    
    if ClientSocket < 0 then
    begin
      if FRunning then
        Sleep(100); // Avoid busy loop on error
      Continue;
    end;
    
    // Handle connection (could spawn a thread here for concurrent handling)
    try
      HandleConnection(ClientSocket);
    except
      on E: Exception do
      begin
        // Log error (for now, silently ignore)
      end;
    end;
    
    CloseSocket(ClientSocket);
  end;
end;

procedure THTTPGRPCServerTransport.HandleConnection(ClientSocket: TSocket);
var
  Method, Path, ContentType: AnsiString;
  ContentLength: Integer;
  RequestData, ResponseData: TByteArray;
  ServiceName, MethodNameStr: AnsiString;
  StatusCode: Integer;
  StatusMessage: AnsiString;
  InitialBodyData: TByteArray;
  RemainingBytes: Integer;
  FramedRequestData: TByteArray;
  UnframedRequestData: TByteArray;
begin
  // Parse HTTP request (may already contain some body data)
  if not ParseHTTPRequest(ClientSocket, Method, Path, ContentType, ContentLength, InitialBodyData) then
  begin
    SendHTTPResponse(ClientSocket, 400, 'Bad Request', nil);
    Exit;
  end;
  
  // Only support POST
  if Method <> 'POST' then
  begin
    SendHTTPResponse(ClientSocket, 405, 'Method Not Allowed', nil);
    Exit;
  end;
  
  // Check if this is a gRPC-Web request
  FUseGRPCWeb := (Pos('grpc-web', LowerCase(ContentType)) > 0);
  
  // Read remaining request body if needed
  RemainingBytes := ContentLength - Length(InitialBodyData);
  if RemainingBytes > 0 then
  begin
    RequestData := ReadRequestBody(ClientSocket, RemainingBytes);
    // Prepend the initial body data
    if Length(InitialBodyData) > 0 then
    begin
      SetLength(FramedRequestData, Length(InitialBodyData) + Length(RequestData));
      Move(InitialBodyData[0], FramedRequestData[0], Length(InitialBodyData));
      Move(RequestData[0], FramedRequestData[Length(InitialBodyData)], Length(RequestData));
    end
    else
      FramedRequestData := RequestData;
  end
  else
    FramedRequestData := InitialBodyData;
  
  // Unframe the gRPC message (remove 5-byte header)
  UnframedRequestData := UnframeMessage(FramedRequestData);
  if UnframedRequestData = nil then
  begin
    SendHTTPResponse(ClientSocket, 400, 'Invalid gRPC frame', nil);
    Exit;
  end;
  
  // Parse path to get service and method names
  ParsePath(Path, ServiceName, MethodNameStr);
  
  // Call request handler with UNFRAMED data
  ResponseData := nil;
  if Assigned(FRequestHandler) then
    FRequestHandler(ServiceName, MethodNameStr, UnframedRequestData, 
      ResponseData, StatusCode, StatusMessage);
  
  // Send response
  if StatusCode = 0 then
    SendHTTPResponse(ClientSocket, 200, 'OK', ResponseData)
  else
    SendHTTPResponse(ClientSocket, 500, StatusMessage, nil);
end;

function THTTPGRPCServerTransport.ParseHTTPRequest(ClientSocket: TSocket;
  out Method, Path, ContentType: AnsiString; out ContentLength: Integer; out BodyData: TByteArray): Boolean;
var
  Buffer: array[0..4095] of Char;
  Line: AnsiString;
  Lines: TStringList;
  i, BytesRead, HeaderEndPos: Integer;
  RequestLine: AnsiString;
  Parts: TStringList;
  HeaderLine: AnsiString;
  RawData: AnsiString;
begin
  Result := False;
  Method := '';
  Path := '';
  ContentType := '';
  ContentLength := 0;
  BodyData := nil;
  
  // Read headers (and possibly some body)
  FillChar(Buffer, SizeOf(Buffer), 0);
  BytesRead := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer) - 1, 0);
  
  if BytesRead <= 0 then
    Exit;
  
  // Convert to string for parsing
  SetLength(RawData, BytesRead);
  Move(Buffer[0], RawData[1], BytesRead);
  
  // Find end of headers (double CRLF)
  HeaderEndPos := Pos(#13#10#13#10, RawData);
  if HeaderEndPos = 0 then
    HeaderEndPos := BytesRead
  else
    HeaderEndPos := HeaderEndPos + 3; // Position after CRLF CRLF
  
  Lines := TStringList.Create;
  try
    Lines.Text := Copy(RawData, 1, HeaderEndPos);
    if Lines.Count < 1 then
      Exit;
    
    // Parse request line: "POST /ServiceName/MethodName HTTP/1.1"
    RequestLine := Lines[0];
    Parts := TStringList.Create;
    try
      Parts.Delimiter := ' ';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := RequestLine;
      
      if Parts.Count < 3 then
        Exit;
      
      Method := UpperCase(Parts[0]);
      Path := Parts[1];
    finally
      Parts.Free;
    end;
    
    // Parse headers
    for i := 1 to Lines.Count - 1 do
    begin
      HeaderLine := Lines[i];
      if Pos('Content-Length:', HeaderLine) = 1 then
      begin
        Delete(HeaderLine, 1, Length('Content-Length:'));
        HeaderLine := Trim(HeaderLine);
        ContentLength := StrToIntDef(HeaderLine, 0);
      end
      else if Pos('Content-Type:', HeaderLine) = 1 then
      begin
        Delete(HeaderLine, 1, Length('Content-Type:'));
        ContentType := Trim(HeaderLine);
      end;
      
      // Stop at empty line (end of headers)
      if Trim(HeaderLine) = '' then
        Break;
    end;
    
    // Extract any body data that was read with the headers
    if (HeaderEndPos < BytesRead) and (ContentLength > 0) then
    begin
      SetLength(BodyData, BytesRead - HeaderEndPos);
      Move(Buffer[HeaderEndPos], BodyData[0], BytesRead - HeaderEndPos);
    end;
    
    Result := True;
  finally
    Lines.Free;
  end;
end;

function THTTPGRPCServerTransport.ReadRequestBody(ClientSocket: TSocket;
  ContentLength: Integer): TByteArray;
var
  BytesRead, TotalRead: Integer;
  Buffer: array[0..4095] of Byte;
begin
  Result := nil;
  if ContentLength <= 0 then
    Exit;
  
  SetLength(Result, ContentLength);
  TotalRead := 0;
  
  while TotalRead < ContentLength do
  begin
    BytesRead := fpRecv(ClientSocket, @Buffer, 
      Min(SizeOf(Buffer), ContentLength - TotalRead), 0);
    
    if BytesRead <= 0 then
      Break;
    
    Move(Buffer, Result[TotalRead], BytesRead);
    TotalRead := TotalRead + BytesRead;
  end;
  
  if TotalRead <> ContentLength then
    SetLength(Result, TotalRead);
end;

procedure THTTPGRPCServerTransport.SendHTTPResponse(ClientSocket: TSocket;
  StatusCode: Integer; const StatusMessage: AnsiString;
  const ResponseData: TByteArray);
var
  Response: AnsiString;
  FullResponseBody: TByteArray;
  DataFrameLen: UInt32;
  TrailerFrame: TByteArray;
  TrailerText: AnsiString;
  Pos: Integer;
begin
  // Build gRPC-Web response with data frame + trailer frame
  
  DataFrameLen := Length(ResponseData);
  
  // Allocate full response: data frame (5 bytes header + data) + trailer frame
  SetLength(FullResponseBody, 5 + DataFrameLen);
  Pos := 0;
  
  // Data frame header (not a trailer)
  FullResponseBody[Pos] := 0; // Compression flag = 0, Trailer flag = 0
  Inc(Pos);
  
  // Message length (4 bytes, big-endian)
  FullResponseBody[Pos] := Byte((DataFrameLen shr 24) and $FF);
  FullResponseBody[Pos + 1] := Byte((DataFrameLen shr 16) and $FF);
  FullResponseBody[Pos + 2] := Byte((DataFrameLen shr 8) and $FF);
  FullResponseBody[Pos + 3] := Byte(DataFrameLen and $FF);
  Inc(Pos, 4);
  
  // Copy message data
  if DataFrameLen > 0 then
  begin
    Move(ResponseData[0], FullResponseBody[Pos], DataFrameLen);
    Inc(Pos, DataFrameLen);
  end;
  
  // Build trailer frame (gRPC status in text format)
  TrailerText := Format('grpc-status: %d'#13#10'grpc-message: %s'#13#10, 
    [0, 'OK']);
  
  SetLength(TrailerFrame, 5 + Length(TrailerText));
  
  // Trailer frame header (with trailer flag set)
  TrailerFrame[0] := $80; // Trailer flag = 1
  
  // Trailer length (4 bytes, big-endian)
  TrailerFrame[1] := Byte((Length(TrailerText) shr 24) and $FF);
  TrailerFrame[2] := Byte((Length(TrailerText) shr 16) and $FF);
  TrailerFrame[3] := Byte((Length(TrailerText) shr 8) and $FF);
  TrailerFrame[4] := Byte(Length(TrailerText) and $FF);
  
  // Copy trailer text
  if Length(TrailerText) > 0 then
    Move(TrailerText[1], TrailerFrame[5], Length(TrailerText));
  
  // Append trailer frame to response body
  SetLength(FullResponseBody, Length(FullResponseBody) + Length(TrailerFrame));
  Move(TrailerFrame[0], FullResponseBody[Pos], Length(TrailerFrame));
  
  // Build HTTP response with gRPC-Web content type
  Response := Format('HTTP/1.1 %d %s'#13#10, [StatusCode, StatusMessage]);
  Response := Response + 'Content-Type: application/grpc-web+proto'#13#10;
  Response := Response + Format('Content-Length: %d'#13#10, [Length(FullResponseBody)]);
  Response := Response + 'X-Grpc-Web: 1'#13#10;
  Response := Response + 'Access-Control-Allow-Origin: *'#13#10;  // CORS support
  Response := Response + 'Connection: close'#13#10;
  Response := Response + #13#10; // End of headers
  
  // Send headers
  fpSend(ClientSocket, @Response[1], Length(Response), 0);
  
  // Send full response body (data + trailers)
  if Length(FullResponseBody) > 0 then
    fpSend(ClientSocket, @FullResponseBody[0], Length(FullResponseBody), 0);
end;

procedure THTTPGRPCServerTransport.ParsePath(const Path: AnsiString;
  out ServiceName, MethodNameStr: AnsiString);
var
  Parts: TStringList;
begin
  ServiceName := '';
  MethodNameStr := '';
  
  // Path format: /ServiceName/MethodName or /package.ServiceName/MethodName
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Path;
    
    // Remove empty parts
    while (Parts.Count > 0) and (Parts[0] = '') do
      Parts.Delete(0);
    
    if Parts.Count >= 2 then
    begin
      ServiceName := Parts[0];
      MethodNameStr := Parts[1];
    end
    else if Parts.Count = 1 then
    begin
      // Maybe just method name?
      MethodNameStr := Parts[0];
    end;
  finally
    Parts.Free;
  end;
end;

function THTTPGRPCServerTransport.UnframeMessage(const FramedData: TByteArray): TByteArray;
var
  MessageLen: UInt32;
  i: Integer;
begin
  Result := nil;
  
  // gRPC framing: 1 byte compression flag + 4 bytes message length (big-endian) + message
  if Length(FramedData) < 5 then
    Exit;
  
  // Check compression flag (must be 0 for uncompressed)
  // For now, we accept any compression flag value
  
  // Read message length (big-endian)
  MessageLen := (UInt32(FramedData[1]) shl 24) or
                (UInt32(FramedData[2]) shl 16) or
                (UInt32(FramedData[3]) shl 8) or
                UInt32(FramedData[4]);
  
  // Verify we have enough data
  if Length(FramedData) < 5 + Integer(MessageLen) then
    Exit;
  
  // Extract the message
  SetLength(Result, MessageLen);
  if MessageLen > 0 then
    Move(FramedData[5], Result[0], MessageLen);
end;

end.
