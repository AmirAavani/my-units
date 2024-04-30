unit StepHandlersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PipelineUnit, Pipeline.TypesUnit;

function Step1Hanlder(Task: TTask): Boolean;
function Steps2And3Hanlder(Task: TTask): Boolean;

implementation
uses
  ALoggerUnit, Time, DataUnit, GenericCollectionUnit;
type
  TObjectList = specialize TObjectCollection<TObject>;


function Step1Hanlder(Task: TTask): Boolean;
var
  SleepTime: Integer;

begin
  ALoggerUnit.GetLogger.FMTDebugLn('Step 1 Task: %d', [Task.ID]);
  SleepTime := Random(1000);
  ALoggerUnit.GetLogger.FMTDebugLn('Step 1 Task: %d Sleeping for %dms', [Task.ID, SleepTime]);
  Sleep(SleepTime);

  ALoggerUnit.GetLogger.FMTDebugLn('Done Step 1 Task: %d', [Task.ID]);

  Result := True;

end;

function CreateStep2Args(Task: TTask): TObjectList;
var
  Data: TData;

begin
  Data := TData.CreateInt64(Task.ID * 10 + 2);
  Result := TObjectList.Create;
  Result.Add(Data);

end;

function CreateStep3Args(Task: TTask): TObjectList;
var
  Data: TData;

begin
  Data := TData.CreateInt64(Task.ID * 10 + 3);
  Result := TObjectList.Create;
  Result.Add(Data);

end;

function Steps2And3Hanlder(Task: TTask): Boolean;
var
  SleepTime: Integer;
  StepID: Integer;
  Args: TObjectList;

begin
  if Task.StepInfo.ID = 2 then
    Args := CreateStep2Args(Task)
  else if Task.StepInfo.ID = 3 then
    Args := CreateStep3Args(Task)
  else
    ALoggerUnit.FmtFatalLnIFFalse(False, 'Invalid StepInfo.ID: %d', [Task.StepInfo.ID]);
  StepID := Task.StepInfo.ID;

  SleepTime := Random(1000);
  ALoggerUnit.GetLogger.FMTDebugLn('Step %d Task: %d  with Arg %d Sleeping for %dms',
    [StepID, Task.ID, (Args[0].Create as TData).DataAsUInt64, SleepTime]);
  Sleep(SleepTime);

  ALoggerUnit.GetLogger.FMTDebugLn('Done Step %d Task: %d', [StepID, Task.ID]);

  Result := True;
  Args.Free;
end;

end.

