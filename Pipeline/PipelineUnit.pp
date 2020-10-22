unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  ThreadSafeQueueUnit, fgl, Classes, SysUtils;

type
  TTask = class;
  TDoneTaskQueue = specialize TThreadSafeQueue<PInteger>;
  TStepHandler = function (Task: TTask): Boolean;

  { TPipeline }

  TPipeline = class(TObject)
  private type
    { TStepInfo }

    TStepInfo = class(TObject)
    private
      NumTasks: Integer;
      FTaskID: Integer;
      StepHandler: TStepHandler;

    public
      constructor Create(_TaskID: Integer; _NumTasks: Integer; Handler: TStepHandler);
      destructor Destroy; override;

    end;

    TStepInfoList = specialize TFPGList<TStepInfo>;

  private
    FName: AnsiString;
    Steps: TStepInfoList;

  protected
    function RunStep(Step: TStepInfo): Boolean;

  public
    property Name: AnsiString read FName;

    constructor Create(_Name: AnsiString);
    destructor Destroy; override;

    procedure AddNewStep(Handler: TStepHandler; NumTasks: Integer);

    function Run: Boolean;
    function RunStep(StepIndex: Integer): Boolean;
    function RunFromStep(StepIndex: Integer): Boolean;
  end;


  { TTask }

  TTask = class(TObject)
  private
    FID: Integer;
    FCount: Integer;
    FStep: TPipeline.TStepInfo;

  public
    property ID: Integer read FID;
    property Count: Integer read FCount;
    property StepInfo: TPipeline.TStepInfo read FStep;

    constructor Create(_ID: Integer; _Step: TPipeline.TStepInfo);

  end;

implementation
uses
  SyncUnit, ALoggerUnit, RunInAThreadUnit, Crt;

{ TTask }

constructor TTask.Create(_ID: Integer; _Step: TPipeline.TStepInfo);
begin
  inherited Create;

  FID := _ID;
  FStep := _Step;


end;

{ TPipeline.TStepInfo }

constructor TPipeline.TStepInfo.Create(_TaskID: Integer; _NumTasks: Integer;
  Handler: TStepHandler);
begin
  inherited Create;

  NumTasks := _NumTasks;
  StepHandler := Handler;

end;

destructor TPipeline.TStepInfo.Destroy;
begin
  inherited Destroy;

end;

function RunHandler(Args: array of Pointer): Boolean;
var
  Task: TTask;
  Queue: TDoneTaskQueue;
  wg: TWaitGroup;

begin
  Task := TTask(Args[0]);
  Queue := TDoneTaskQueue(Args[1]);
  wg := TWaitGroup(Args[2]);
  DebugLn(Format('Running Task %d', [Task.ID]));
  DebugLn(IntToStr(Task.ID));
  DebugLn(IntToStr(Task.ID));

  Result := Task.StepInfo.StepHandler(Task);

  DebugLn(IntToStr(Task.ID));
  DebugLn(Format('%d: Inserting %d into Queue', [ThreadID, Task.ID]));
  Queue.Insert(PInteger(@Task.ID));

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
  Args: TArrayOfPointer;
  Status: array of Boolean;
  Thread: TThread;
  Wg: TWaitGroup;

begin
  Queue := TDoneTaskQueue.Create;
  AllTasks := TTasks.Create;
  SetLength(Status, Step.NumTasks + 1);

  Wg := TWaitGroup.Create;
  Wg.Add(Step.NumTasks);

  for i := 1 to Step.NumTasks do
  begin
    SetLength(Args, 3);
    AllTasks.Add(TTask.Create(i, Step));
    Args[0] := AllTasks.Last;
    Args[1] := Queue;
    Args[2] := wg;

    RunInThread(@RunHandler, Args, @Status[i]);

  end;

  DebugLn(Format('Waiting for all jobs of Task %d to Finsih', [Step.FTaskID]));
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

procedure TPipeline.AddNewStep(Handler: TStepHandler; NumTasks: Integer);
begin
  Steps.Add(TStepInfo.Create(Steps.Count, NumTasks, Handler));

end;

function TPipeline.Run: Boolean;
begin
  Result := RunFromStep(1);

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

