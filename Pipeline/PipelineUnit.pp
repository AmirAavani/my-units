unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pipeline.TypesUnit, ThreadSafeQueueUnit, GenericCollectionUnit,
  ThreadPoolUnit;

type
  TTask = class;
  TStepHandler = function (Task: TTask): Boolean;

  { TPipelineConfig }

  TPipelineConfig = class(TObject)
  private
    FNumberOfThreads: Integer;
  public
    property NumberOfThreads: Integer read FNumberOfThreads;

    constructor Create;

    class function DefaultConfig: TPipelineConfig;
    function SetNumberOfThreads(x: Integer): TPipelineConfig;

  end;

  { TPipeline }

  TPipeline = class(TObject)
  private type
    { TStepInfo }

    TStepInfo = class(TObject)
    private
      FNumTasks: Integer;
      FID: Integer;
      StepHandler: TStepHandler;

    public
      property ID: Integer read FID;
      property NumTasks: Integer read FNumTasks;

      constructor Create(_StepID: Integer; _NumTasks: Integer;
        Handler: TStepHandler);
      destructor Destroy; override;

    end;

    TStepInfoList = specialize TObjectCollection<TStepInfo>;

  private
    FName: AnsiString;
    Steps: TStepInfoList;

  protected
    ThreadPool: TThreadPool;
    Config: TPipelineConfig;

    function RunStep(Step: TStepInfo): Boolean;

    function RunStep(StepIndex: Integer): Boolean;
    function RunFromStep(StepIndex: Integer): Boolean;
  public
    property Name: AnsiString read FName;

    constructor Create(_Name: AnsiString; _Config: TPipelineConfig);
    destructor Destroy; override;

    procedure AddNewStep(Handler: TStepHandler; NumTasks: Integer);

    function Run: Boolean;
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
  SyncUnit, StringUnit, ALoggerUnit, RunInAThreadUnit, ParameterManagerUnit,
  Pipeline.Utils;

{ TPipelineConfig }

constructor TPipelineConfig.Create;
begin
  inherited Create;

  FNumberOfThreads := 16;

end;

class function TPipelineConfig.DefaultConfig: TPipelineConfig;
begin
  Result := TPipelineConfig.Create;

end;

function TPipelineConfig.SetNumberOfThreads(x: Integer): TPipelineConfig;
begin
  FNumberOfThreads := x;

  Result := Self;

end;

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

constructor TPipeline.TStepInfo.Create(_StepID: Integer; _NumTasks: Integer;
  Handler: TStepHandler);
begin
  inherited Create;

  FID := _StepID;
  FNumTasks := _NumTasks;
  StepHandler := Handler;

end;

destructor TPipeline.TStepInfo.Destroy;
begin
  inherited Destroy;

end;

function RunHandler(SysArgs: TObjectList): Boolean;
var
  Task: TTask;
  wg: TWaitGroup;
  Step: TPipeline.TStepInfo;

begin
  Task := TTask(SysArgs[0]);
  wg := TWaitGroup(SysArgs[1]);
  Step := TPipeline.TStepInfo(SysArgs[2]);
  FMTDebugLn('Running Task %d', [Task.ID], 5);

  FMTDebugLn('StepID: %d TaskID: %d', [Step.ID, Task.ID], 5);

  Result := Task.StepInfo.StepHandler(Task);

  FMTDebugLn('wg.Done Task: %d Result: %s', [Task.ID, BoolToStr(Result, True)], 5);

  wg.Done(1);

  SysArgs.Clear;
  SysArgs.Free;

end;

{ TPipeline }

function TPipeline.RunStep(Step: TStepInfo): Boolean;
type
  TTasks = specialize TObjectCollection<TTask>;

var
  i: Integer;
  AllTasks: TTasks;
  SysArgs: TObjectList;
  Status: array of Boolean;
  Wg: TWaitGroup;

begin
  if Step = nil then
    FmtFatalLn('Invalid Step', []);

  AllTasks := TTasks.Create;
  SetLength(Status, Step.NumTasks + 1);

  Wg := TWaitGroup.Create;
  Wg.Add(Step.NumTasks);

  for i := 1 to Step.NumTasks do
  begin
    AllTasks.Add(TTask.Create(i, Step));

    SysArgs := TObjectList.Create;
    SysArgs.Count := 3;
    SysArgs[0] := AllTasks.Last;
    SysArgs[1] := wg;
    SysArgs[2] := Step;

    //RunInThread(@RunHandler, SysArgs, @Status[i]);
    ThreadPool.Run(@RunHandler, SysArgs, @Status[i]);
  end;

  FMTDebugLn('Waiting for all jobs of Step %d to Finish', [Step.ID], 5);
  Wg.Wait();
  FMTDebugLn('Step: %d All are Done', [Step.ID], 5);

  DebugLn('All jobs are Running');

  for i := 1 to Step.NumTasks do
  begin
    FMTDebugLn('Task %d is Done with Status: %s', [i, BoolToStr(Status[i], True)], 5);
  end;

  SetLength(Status, 0);
  AllTasks.Free;
  Wg.Free;
  Result := True;
end;

constructor TPipeline.Create(_Name: AnsiString; _Config: TPipelineConfig);
begin
  inherited Create;

  FName := _Name;
  Steps := TStepInfoList.Create;
  Steps.Add(nil);
  Config := _Config;
  ThreadPool := TThreadPool.Create(Config.NumberOfThreads);

end;

destructor TPipeline.Destroy;
begin
  Steps.Free;

  inherited Destroy;
end;

procedure TPipeline.AddNewStep(Handler: TStepHandler; NumTasks: Integer);
begin
  Steps.Add(TStepInfo.Create(Steps.Count, NumTasks, Handler));

end;

function RunThePipeline(SysArgs: TObjectList): Boolean;
var
  ThePipeline: TPipeline;
  StepID, FromStepID: Integer;
  wg: TWaitGroup;
  ToStepID, ID: Integer;

begin
  ThePipeline := SysArgs[0] as TPipeline;
  wg := TWaitGroup(SysArgs[1]);

  Result := False;
  StepID := GetRunTimeParameterManager.ValueByName['Pipeline.StepID'].AsIntegerOrDefault(-1);
  FromStepID := GetRunTimeParameterManager.ValueByName['Pipeline.FromStepID'].AsIntegerOrDefault(1);
  ToStepID := ThePipeline.Steps.Count - 1;
  if StepID <> -1 then
  begin
    FromStepID := StepID;
    ToStepID := StepID;

  end;

  for ID := FromStepID to ToStepID do
    Result := ThePipeline.RunStep(ID);

  FMTDebugLn('Result: %s', [BoolToStr(Result, True)], 5);

  wg.Done(1);

end;

function TPipeline.Run: Boolean;
var
  SysArgs: TObjectList;
  wg: TWaitGroup;

begin
  SysArgs := TObjectList.Create;
  SysArgs.Add(Self);
  wg := TWaitGroup.Create;
  SysArgs.Add(wg);

  Result := False;
  wg.Add(1);
  RunInThread(@RunThePipeline, SysArgs, @Result);
  wg.Wait;

  wg.Free;
  SysArgs.Clear;
  SysArgs.Free;

end;

function TPipeline.RunStep(StepIndex: Integer): Boolean;
begin
  FMTDebugLn('%d StepIndex: %d', [ThreadID, StepIndex], 5);

  Result := RunStep(Steps[StepIndex]);

  FMTDebugLn('%d Result: %s', [ThreadID, BoolToStr(Result, True)], 5);

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

