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

    class function Run(aPipeline: TPipeline): Boolean;
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
//   Queue: TDoneTaskQueue;
  wg: TWaitGroup;

begin
  Task := TTask(SysArgs[0]);
  // Queue := TDoneTaskQueue(SysArgs[1]);
  wg := TWaitGroup(SysArgs[2]);
  DebugLn(Format('Running Task %d', [Task.ID]));

  Result := Task.StepInfo.StepHandler(Task, Args);

  DebugLn(Format('TaskID: %d', [Task.ID]));
  DebugLn(Format('%d: Inserting %d into Queue', [ThreadID, Task.ID]));
  // Queue.Insert(PInteger(@Task.ID));
  DebugLn(Format('%d: %d wg.Done', [ThreadID, Task.ID]));

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
  DebugLn(Format('All are Done', [Step.FTaskID]));

  DebugLn('All jobs are Running');

  for i := 1 to Step.NumTasks do
  begin
    DebugLn(Format('Task %d?', [i]));
    DebugLn(Format('Before Delete', []));
    Queue.Delete(TaskID);

    DebugLn(Format('%d Task %d is Done with Status: %s', [i, TaskID^,
      BoolToStr(Status[TaskID^], 'True', 'False')]));
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

begin
  ThePipeline := TPipeline(SysArgs[0]);
  PStepID := PInteger(SysArgs[1]);
  PFromStepID := PInteger(SysArgs[2]);

  if PStepID^ <> -1 then
    ThePipeline.RunStep(PStepID^)
  else
    ThePipeline.RunFromStep(PFromStepID^);

end;

class function TPipeline.Run(aPipeline: TPipeline): Boolean;
var
  SysArgs: TPointerArray;
  StepID, FromStepID: Integer;
  Done: Boolean;

begin

  SetLength(SysArgs, 4);
  SysArgs[0] := aPipeline;
  StepID := GetRunTimeParameterManager.ValueByName['--Pipeline.StepID'].AsInteger;
  FromStepID := GetRunTimeParameterManager.ValueByName['--Pipeline.FromStepID'].AsIntegerOrDefault(1);
  SysArgs[1] := PInteger(@StepID);
  SysArgs[2] := PInteger(@FromStepID);
  SysArgs[3] := @Done;

  Done := False;
  RunInThread(@RunThePipeline, SysArgs, nil, @Result);

  while not Done do
  begin
    Sleep(10000);
    DebugLn('What''s up!');
  end;

end;

function TPipeline.RunStep(StepIndex: Integer): Boolean;
begin
  DebugLn(Format('%d StepIndex: %d', [ThreadID, StepIndex]));

  Result := RunStep(Steps[StepIndex]);

  DebugLn(Format('%d Result: %s', [ThreadID, BoolToStr(Result, 'True', 'False')]));

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

