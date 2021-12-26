program Test01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, ThreadPoolUnit, GenericCollectionUnit, HeapUnit, ALoggerUnit,
  WideStringUnit, DataUnit, sysutils
  { you can add units after this };

function F(Args: TObjectList): Boolean;
var
  Name: AnsiString;
  WaitTime: Integer;

begin
  Name := (Args[0] as TData).DataAsString;
  WaitTime := (Args[1] as TData).DataAsUInt64;

  FMTDebugLn('+Name: %s WaitTime: %d', [Name, WaitTime]);
  Sleep(WaitTime );
  FMTDebugLn('-Name: %s', [Name]);
  Args.Free;

end;

var
  Pool: TThreadPool;
  Args: TObjectList;
  i: Integer;

begin
  Pool := TThreadPool.Create(16);

  for i := 1 to 64 do
  begin
    Args := TObjectList.Create;
    Args.Add(TData.CreateString('Run' + IntToStr(i)));
    Args.Add(TData.CreateUInt64(i * 100));

    FMTDebugLn('Adding', []);
    Pool.Run(@F, Args, nil)
  end;
  Pool.Wait;
  Pool.Free;
end.

