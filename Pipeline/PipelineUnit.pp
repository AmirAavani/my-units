unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  Pipeline.Types, ThreadSafeQueueUnit, fgl, Classes, SysUtils;

type
  TTask = class;
  TDoneTaskQueue = specialize TThreadSafeQueue<PInteger>;
  TStepHandler = function (Task: TTask; Args: TPointerArray): Boolean;

  { TPipeline }

  TPipeline = class(TObject)
  private type
    { TStepInfo }

    TStepInfo = class(TObject)
    private
      NumTasks: Integer;
      FTaskID: Integer;
      StepHandler: TStepHandler;
      FArgs: TPointerArray;

    public
      constructor Create(_TaskID: Integer; _NumTasks: Integer;
          Handler: TStepHandler; _Args: TPointerArray);
      destructor Destroy; override;

    end;

    TStepInfoList = specialize TFPGList<TStepInfo>;

  private
    FName: AnsiString;
    Steps: TStepInfoList;

  protected
    function RunStep(Step: TStepInfo): Boolean;

  function RunStep(StepIndex: Integer): Boolean;
  function RunFromStep(StepIndex: Integer): Boolean;
  public
    property Name: AnsiString read FName;

    constructor Create(_Name: AnsiString);
    destructor Destroy; override;

    procedure AddNewStep(Handler: TStepHandler; NumTasks: Integer; Args: TPointerArray);

    class function Run(aPipeline: TPipeline; WaitToFinish: Boolean = True): Boolean;
  end;

  { TTask }

  TTask = class(TObject)
  private
    FID: Integer;
    FStep: TPipeline.TStepInfo;
    function GetCount: Integer;

  public
    property ID: Integer read FID;
    property Count: Integer read GetCount;
    property StepInfo: TPipeline.TStepInfo read FStep;

    constructor Create(_ID: Integer; _Step: TPipeline.TStepInfo);

  end;

implementation
uses
  SyncUnit, ALoggerUnit, RunInAThreadUnit, ParameterManagerUnit;

{ TTask }

function TTask.GetCount: Integer;
begin
  Result := StepInfo.NumTasks;

end;

constructor TTask.Create(_ID: Integer; _Step: TPipeline.TStepInfo);
begin
  inherited Create;

  FID := _ID;
  FStep := _Step;

end;

{ TPipeline.TStepInfo }

constructor TPipeline.TStepInfo.Create(_TaskID: Integer; _NumTasks: Integer;
  Handler: TStepHandler; _Args: TPointerArray);
begin
  inherited Create;

  NumTasks := _NumTasks;
  StepHandler := Handler;
  FArgs := _Args;
end;

destructor TPipeline.TStepInfo.Destroy;
begin
  inherited Destroy;

end;

function RunHandler(SysArgs, Args: TPointerArray): Boolean;
var
  Task: TTask;
  Queue: TDoneTaskQueue;
  wg: TWaitGroup;

begin
  Task := TTask(SysArgs[0]);
  Queue := TDoneTaskQueue(SysArgs[1]);
  wg := TWaitGroup(SysArgs[2]);
  FMTDebugLn('Running Task %d', [Task.ID]);

  Result := Task.StepInfo.StepHandler(Task, Args);

  FMTDebugLn('TaskID: %d', [Task.ID]);
  FMTDebugLn('%d: Inserting %d into Queue', [ThreadID, Task.ID]);
  Queue.Insert(PInteger(@Task.ID));
  FMTDebugLn('%d: %d wg.Done', [ThreadID, Task.ID]);

  wg.Done(1);
end;

{ TPipeline }

function TPipeline.RunStep(Step: TStepInfo): Boolean;
type
  TTasks = specialize TFPGList<TTask>;

var
  i: Integer;
  Queue: TDoneTaskQueue;
  TaskID: PInteger;
  AllTasks: TTasks;
  SysArgs: TPointerArray;
  Status: array of Boolean;
  Wg: TWaitGroup;

begin
  Queue := TDoneTaskQueue.Create;
  AllTasks := TTasks.Create;
  SetLength(Status, Step.NumTasks + 1);

  Wg := TWaitGroup.Create;
  Wg.Add(Step.NumTasks);

  SysArgs := nil;
  SetLength(SysArgs, 3);
  for i := 1 to Step.NumTasks do
  begin
    AllTasks.Add(TTask.Create(i, Step));
    SysArgs[0] := AllTasks.Last;
    SysArgs[1] := Queue;
    SysArgs[2] := wg;

    RunInThread(@RunHandler, SysArgs, Step.FArgs, @Status[i]);

  end;

  FMTDebugLn('Waiting for all jobs of Task %d to Finsih', [Step.FTaskID]);
  Wg.Wait();
  FMTDebugLn('All are Done', [Step.FTaskID]);

  DebugLn('All jobs are Running');

  for i := 1 to Step.NumTasks do
  begin
    FMTDebugLn('Task %d?', [i]);
    DebugLn('Before Delete');
    Queue.Delete(TaskID);

    FMTDebugLn('%d Task %d is Done with Status: %s', [i, TaskID^,
      BoolToStr(Status[TaskID^], True)]);
  end;

  for i := 0 to AllTasks.Count - 1 do
    AllTasks[i].Free;
  AllTasks.Free;
  Queue.Free;
  wg.Free;

  Result := True;
end;

constructor TPipeline.Create(_Name: AnsiString);
begin
  inherited Create;

  FName := _Name;
  Steps := TStepInfoList.Create;
  Steps.Add(nil);

end;

destructor TPipeline.Destroy;
var
  Step: TStepInfo;

begin
  for Step in Steps do
    Step.Free;

  Steps.Free;

  inherited Destroy;
end;

procedure TPipeline.AddNewStep(Handler: TStepHandler; NumTasks: Integer;
  Args: TPointerArray);
begin
  Steps.Add(TStepInfo.Create(Steps.Count, NumTasks, Handler, Args));

end;

function RunThePipeline(SysArgs, Args: TPointerArray): Boolean;
var
  ThePipeline: TPipeline;
  PStepID, PFromStepID: PInteger;
  wg: TWaitGroup;

begin
  ThePipeline := TPipeline(SysArgs[0]);
  PStepID := PInteger(SysArgs[1]);
  PFromStepID := PInteger(SysArgs[2]);
  wg := TWaitGroup(SysArgs[3]);
  Result := False;

  if PStepID^ <> -1 then
    Result := ThePipeline.RunStep(PStepID^)
  else
    Result := ThePipeline.RunFromStep(PFromStepID^);
  FMTDebugLn('Result: %s', [BoolToStr(Result, True)]);

  wg.Done(1);

end;

class function TPipeline.Run(aPipeline: TPipeline; WaitToFinish: Boolean
  ): Boolean;
var
  SysArgs: TPointerArray;
  StepID, FromStepID: Integer;
  wg: TWaitGroup;
  Done: Boolean;

begin
  SetLength(SysArgs, 4);
  SysArgs[0] := aPipeline;
  StepID := GetRunTimeParameterManager.ValueByName['--Pipeline.StepID'].AsInteger;
  FromStepID := GetRunTimeParameterManager.ValueByName['--Pipeline.FromStepID'].AsIntegerOrDefault(1);
  SysArgs[1] := PInteger(@StepID);
  SysArgs[2] := PInteger(@FromStepID);
  wg := TWaitGroup.Create;
  SysArgs[3] := wg;

  wg.Add(1);
  Done := False;
  RunInThread(@RunThePipeline, SysArgs, nil, @Done);

  if WaitToFinish then
  begin
    wg.Wait;
  end;

  Result := True;

end;

function TPipeline.RunStep(StepIndex: Integer): Boolean;
begin
  FMTDebugLn('%d StepIndex: %d', [ThreadID, StepIndex]);

  Result := RunStep(Steps[StepIndex]);

  FMTDebugLn('%d Result: %s', [ThreadID, BoolToStr(Result, True)]);

end;

function TPipeline.RunFromStep(StepIndex: Integer): Boolean;
var
  Step: Integer;

begin
  for Step := StepIndex to Steps.Count - 1 do
    if not RunStep(Step) then
      Exit(False);

  Result := True;


end;

end.

