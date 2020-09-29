unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  QueueUnit, TaskUnit, fgl, Classes, SysUtils;

type
  TTask = class;
  TDoneTaskQueue = specialize TThreadSafeQueue<PInteger>;
  TStepHandler = function (Task: TTask; Args: array of Pointer): Boolean;

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
    ID: Integer;
    Step: TPipeline.TStepInfo;

  public
    constructor Create(_ID: Integer; _Step: TPipeline.TStepInfo);

  end;

implementation
uses
  {ThreadPoolUnit, }ALoggerUnit, RunInAThreadUnit;

{ TTask }

constructor TTask.Create(_ID: Integer; _Step: TPipeline.TStepInfo);
begin
  inherited Create;

  ID := _ID;
  Step := _Step;


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

  Task.Step.StepHandler(Task, Args);

  Queue.Insert(PInteger(@Task.ID));


end;

{ TPipeline }

function TPipeline.RunStep(Step: TStepInfo): Boolean;
var
  i: Integer;
  Queue: TDoneTaskQueue;
  TaskID: PInteger;
  Args: array of Pointer;

begin
  Queue := TDoneTaskQueue.Create;

  for i := 1 to Step.NumTasks do
  begin
    SetLength(Args, 2);
    Args[0] := TTask.Create(i, Step);
    Args[1] := Queue;

    RunInThread(@RunHandler, Args);

  end;
  for i := 1 to Step.NumTasks do
  begin
    New(TaskID);
    Queue.Delete(TaskID);

    DebugLn(Format('Task %d is done', [TaskID^]));
    Dispose(TaskID);
  end;

  Queue.Free;

  Result := True;
end;

constructor TPipeline.Create(_Name: AnsiString);
begin
  inherited Create;

  FName := _Name;
  Steps := TStepInfoList.Create;

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
  Result := RunStep(Steps[StepIndex]);

end;

function TPipeline.RunFromStep(StepIndex: Integer): Boolean;
var
  Step: Integer;

begin
  for Step := StepIndex to Steps.Count do
    if not RunStep(Step) then
      Exit(False);

  Result := True;


end;

end.

