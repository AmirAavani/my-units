program Test01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, ThreadPoolUnit, GenericCollectionUnit, HeapUnit, ALoggerUnit,
  WideStringUnit, DataUnit, QueueUnit, sysutils, Generics.Collections
  { you can add units after this };

type
  TObjectList = specialize TList<TObject>;

function F(var Args: TObjectList): Boolean;
var
  Name: AnsiString;
  WaitTime: Integer;
  Obj: TObject;

begin
  Name := (Args[0] as TData).DataAsString;
  WaitTime := (Args[1] as TData).DataAsUInt64;

  WriteLn(Format('+Name: %s WaitTime: %d', [Name, WaitTime]));
  Sleep(WaitTime );
  WriteLn(Format('-Name: %s', [Name]));
  for Obj in Args do
    Obj.Free;
  Args.Free;

end;

type
  TThreadPool  = specialize TGenericThreadPool<TObjectList>;

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

    WriteLn(Format('Adding', []));
    Pool.Run(@F, Args, nil)
  end;
  Pool.Wait;
  Pool.Free;
end.

