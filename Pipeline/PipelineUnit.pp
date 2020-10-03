unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  QueueUnit, TaskUnit, fgl, Classes, SysUtils;

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
      StepHandler: TStepHandler;

    public
      constructor Create(_NumTasks: Integer; Handler: TStepHandler);
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
    FStep: TPipeline.TStepInfo;

  public
    property ID: Integer read FID;
    property StepInfo: TPipeline.TStepInfo read FStep;

    constructor Create(_ID: Integer; _Step: TPipeline.TStepInfo);

  end;

implementation
uses
  {ThreadPoolUnit, }ALoggerUnit, RunInAThreadUnit;

{ TTask }

constructor TTask.Create(_ID: Integer; _Step: TPipeline.TStepInfo);
begin
  inherited Create;

  FID := _ID;
  FStep := _Step;


end;

{ TPipeline.TStepInfo }

constructor TPipeline.TStepInfo.Create(_NumTasks: Integer; Handler: TStepHandler
  );
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

begin
  Task := TTask(Args[0]);
  Queue := TDoneTaskQueue(Args[1]);

  Result := Task.StepInfo.StepHandler(Task);

  DebugLn(Format('%d: Inserting %d into Queue', [ThreadID, Task.ID]));
  Queue.Insert(PInteger(@Task.ID));

end;

{ TPipeline }

function TPipeline.RunStep(Step: TStepInfo): Boolean;
var
  i: Integer;
  Queue: TDoneTaskQueue;
  TaskID: PInteger;
  Args: TArrayOfPointer;
  Status: array of Boolean;
  Thread: TThread;

begin
  Queue := TDoneTaskQueue.Create;
  SetLength(Status, Step.NumTasks + 1);

  for i := 1 to Step.NumTasks do
  begin
    SetLength(Args, 2);
    Args[0] := TTask.Create(i, Step);
    Args[1] := Queue;

    RunInThread(@RunHandler, Args, @Status[i]);

  end;
  for i := 1 to Step.NumTasks do
  begin
    DebugLn(Format('%d: Task %d?', [ThreadID, i]));
    DebugLn(Format('%d: Before delete', [ThreadID]));
    Queue.Delete(TaskID);

    DebugLn(Format('%d: Task %d is Done with Status: %s', [ThreadID, TaskID^,
      BoolToStr(Status[TaskID^], 'True', 'False')]));
  end;

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

  inherited Destroy;
end;

procedure TPipeline.AddNewStep(Handler: TStepHandler; NumTasks: Integer);
begin
  Steps.Add(TStepInfo.Create(NumTasks, Handler));

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

