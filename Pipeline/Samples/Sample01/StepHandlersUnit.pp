unit StepHandlersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PipelineUnit, Pipeline.TypesUnit;

function Step1Hanlder(Task: TTask): Boolean;
function Steps2And3Hanlder(Task: TTask): Boolean;

implementation
uses
  ALoggerUnit, Time;

function Step1Hanlder(Task: TTask): Boolean;
var
  SleepTime: Integer;

begin
  FMTDebugLn('Step 1 Task: %d', [Task.ID]);
  SleepTime := Random(1000);
  FMTDebugLn('Step 1 Task: %d Sleeping for %dms', [Task.ID, SleepTime]);
  Sleep(SleepTime);

  FMTDebugLn('Done Step 1 Task: %d', [Task.ID]);

  Result := True;

end;

function CreateStep2Args: TPointerList;
begin
  Result := TPointerList.Create;
  Result.Add(New(PInteger));
  PInteger(Result[0])^ := 2;

end;

function CreateStep3Args: TPointerList;
begin
  Result := TPointerList.Create;
  Result.Add(New(PInteger));
  PInteger(Result[0])^ := 3;

end;

function Steps2And3Hanlder(Task: TTask): Boolean;
var
  SleepTime: Integer;
  StepID: Integer;

begin
  SleepTime := Random(1000);
  FMTDebugLn('Step %d Task: %d Sleeping for %dms', [StepID, Task.ID, SleepTime]);
  Sleep(SleepTime);

  FMTDebugLn('Done Step %d Task: %d', [StepID, Task.ID]);

  Result := True;

end;

end.

