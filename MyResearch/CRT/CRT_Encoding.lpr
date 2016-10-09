program CRT_Encoding;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, gvector, ParameterManagerUnit
  { you can add units after this };


begin
end.

