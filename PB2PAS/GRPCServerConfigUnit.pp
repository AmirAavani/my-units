unit GRPCServerConfigUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TGRPCServerConfig - Configuration for gRPC server }
  TGRPCServerConfig = class
  private
    FPort: Integer;
    FHost: AnsiString;
    FMaxConnections: Integer;
    FTimeout: Integer;
    FUseSSL: Boolean;
    FCertFile: AnsiString;
    FKeyFile: AnsiString;
    
  public
    constructor Create;
    
    // Server binding
    property Host: AnsiString read FHost write FHost;
    property Port: Integer read FPort write FPort;
    
    // Connection limits
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property Timeout: Integer read FTimeout write FTimeout; // milliseconds
    
    // SSL/TLS (future support)
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property CertFile: AnsiString read FCertFile write FCertFile;
    property KeyFile: AnsiString read FKeyFile write FKeyFile;
  end;

implementation

{ TGRPCServerConfig }

constructor TGRPCServerConfig.Create;
begin
  inherited Create;
  FHost := '0.0.0.0'; // Listen on all interfaces
  FPort := 50051;     // Default gRPC port
  FMaxConnections := 100;
  FTimeout := 30000;  // 30 seconds
  FUseSSL := False;
  FCertFile := '';
  FKeyFile := '';
end;

end.
