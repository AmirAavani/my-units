unit GRPCClientConfigUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TGRPCClientConfig }
  TGRPCClientConfig = class
  private
    FHost: string;
    FPort: Integer;
    FUseTLS: Boolean;
    FAuthToken: string;
    FCertificatePath: string;
    FTimeout: Integer;
    FCustomHeaders: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Connection settings
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    
    // TLS settings (for future use)
    property UseTLS: Boolean read FUseTLS write FUseTLS;
    property CertificatePath: string read FCertificatePath write FCertificatePath;
    
    // Authentication (for future use)
    property AuthToken: string read FAuthToken write FAuthToken;
    
    // Other settings
    property Timeout: Integer read FTimeout write FTimeout; // in milliseconds
    property CustomHeaders: TStringList read FCustomHeaders;
  end;

implementation

{ TGRPCClientConfig }

constructor TGRPCClientConfig.Create;
begin
  inherited Create;
  FHost := 'localhost';
  FPort := 50051;
  FUseTLS := False;
  FAuthToken := '';
  FCertificatePath := '';
  FTimeout := 30000; // 30 seconds default
  FCustomHeaders := TStringList.Create;
  FCustomHeaders.NameValueSeparator := ':';
end;

destructor TGRPCClientConfig.Destroy;
begin
  FCustomHeaders.Free;
  inherited Destroy;
end;

end.
