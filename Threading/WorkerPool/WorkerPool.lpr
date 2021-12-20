program WorkerPool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Test01Unit{, Test02Unit};


begin
  Test01;
  // Test02;

end.

