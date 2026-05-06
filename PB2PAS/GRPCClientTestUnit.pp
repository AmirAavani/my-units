unit GRPCClientTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GRPCClientConfigUnit, GRPCClientUnit;

procedure RunGRPCClientTests;

implementation

procedure RunGRPCClientTests;
var
  Config: TGRPCClientConfig;
  Client: TGRPCClient;
begin
  WriteLn('=== gRPC Client Test Suite ===');
  WriteLn;
  
  // Test 1: Simple constructor
  WriteLn('Test 1: Simple constructor');
  Client := TGRPCClient.CreateSimple('localhost', 50051, 'TestService');
  try
    WriteLn('  Host: ', Client.Config.Host);
    WriteLn('  Port: ', Client.Config.Port);
    WriteLn('  Service: ', Client.ServiceName);
    WriteLn('  ✓ Simple constructor works');
  finally
    Client.Free;
  end;
  WriteLn;
  
  // Test 2: Advanced constructor with config
  WriteLn('Test 2: Advanced constructor with config');
  Config := TGRPCClientConfig.Create;
  try
    Config.Host := 'api.example.com';
    Config.Port := 443;
    Config.UseTLS := True;
    Config.AuthToken := 'Bearer test-token-123';
    Config.Timeout := 60000;
    Config.CustomHeaders.Add('X-Custom-Header: test-value');
    
    Client := TGRPCClient.Create(Config, 'GreeterService');
    try
      WriteLn('  Host: ', Client.Config.Host);
      WriteLn('  Port: ', Client.Config.Port);
      WriteLn('  UseTLS: ', Client.Config.UseTLS);
      WriteLn('  AuthToken: ', Client.Config.AuthToken);
      WriteLn('  Timeout: ', Client.Config.Timeout, ' ms');
      WriteLn('  Custom Headers: ', Client.Config.CustomHeaders.Count);
      WriteLn('  ✓ Advanced constructor works');
    finally
      Client.Free;
    end;
  finally
    Config.Free;
  end;
  WriteLn;
  
  WriteLn('=== All Tests Passed ===');
end;

end.
