program TestGRPCClient;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils,
  GRPCClientConfigUnit, GRPCTransportUnit, GRPCClientUnit, GRPCClientTestUnit;

begin
  RunGRPCClientTests;
end.
