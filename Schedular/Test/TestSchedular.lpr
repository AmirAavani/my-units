program TestSchedular;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, SchedularUnit, DataUnit, {RunInAThreadUnit, }ALoggerUnit,
  WideStringUnit, GenericCollectionUnit
  { you can add units after this };

procedure Printer(Args: TObjectList);
var
  pc: PInteger;

begin
  pc := PInteger((Args[1] as TData).DataAsPointer);
  Inc(pc^);
  FMTDebugLn('%d Printer Printing %s', [pc^, (Args[0] as TData).DataAsString]);
end;

var
  Schedular: TSchedular;
  ObjList: TObjectList;
  C1, C2, C3: Integer;

begin
  Schedular := TSchedular.Create;
  C1 := 0; C2 := 0; C3 := 0;


  ObjList := TObjectList.Create;
  ObjList.Add(TData.CreateString('1000'));
  ObjList.Add(TData.CreatePointer(@C1));

  Schedular.AddTask(
    TTaskDefinition.Create
      .SetTaskProcedure(@Printer)
      .SetArguments(ObjList)
      .SetInterval(1000)
    );

  ObjList := TObjectList.Create;
  ObjList.Add(TData.CreateString('1500'));
  ObjList.Add(TData.CreatePointer(@C3));

  Schedular.AddTask(
    TTaskDefinition.Create
      .SetTaskProcedure(@Printer)
      .SetArguments(ObjList)
      .SetInterval(1500)
    );

  ObjList := TObjectList.Create;
  ObjList.Add(TData.CreateString('2000'));
  ObjList.Add(TData.CreatePointer(@C2));

  Schedular.AddTask(
    TTaskDefinition.Create
      .SetTaskProcedure(@Printer)
      .SetArguments(ObjList)
      .SetInterval(2000)
    );

  DebugLn('Before');
  // Sleep(1000);
  DebugLn('After');
  Schedular.Stop;
  Schedular.Free;
end.

