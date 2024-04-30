unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pipeline.TypesUnit, ThreadSafeQueueUnit, GenericCollectionUnit,
  ThreadPoolUnit, ValueUnit, ProtoHelperUnit;

type
  TTask = class;
  TStepHandlerFunc = function(Task: TTask): Boolean;

  { TStepHandler }

  TStepHandler = class(TObject)
  public
    constructor Create;
    class function CreateFromFunction(f: TStepHandlerFunc): TStepHandler;

    function Handle(Task: TTask): Boolean; virtual; abstract;

  end;

  { TFunctionStepHandler }

  TFunctionStepHandler = class(TStepHandler)
  protected
    FFn: TStepHandlerFunc;

  public
    constructor Create(Fn: TStepHandlerFunc);

    function Handle(Task: TTask): Boolean; override;

    class function FromFunc(Fn: TStepHandlerFunc): TFunctionStepHandler;

  end;


  TObjectList = specialize TObjectCollection<TObject>;

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
  public
  type
    { TStepInfo }

    TStepInfo = class(TObject)
    private
      FNumTasks: Integer;
      FID: Integer;
      StepHandler: TStepHandler;
      FArguments: TObjectList;

    public
      property ID: Integer read FID;
      property NumTasks: Integer read FNumTasks;

      constructor Create(_StepID: Integer; _NumTasks: Integer;
        Handler: TStepHandler);
      constructor Create(_StepID: Integer; _NumTasks: Integer;
        Handler: TStepHandler; Arguments: TObjectList);
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

    constructor Create(constref _Name: AnsiString; _Config: TPipelineConfig);
    destructor Destroy; override;

    procedure AddNewStep(Handler: TStepHandler; NumTasks: Integer;
      Arguments: TObjectList = nil);

    function Run(StepID: Integer = -1): Boolean;

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
  SyncUnit, StringUnit, ALoggerUnit, RunInAThreadUnit,
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

{ TStepHandler }

constructor TStepHandler.Create;
begin
  inherited;

end;

class function TStepHandler.CreateFromFunction(f: TStepHandlerFunc
  ): TStepHandler;
begin
  Result := TFunctionStepHandler.Create(f);

end;

{ TFunctionStepHandler }

constructor TFunctionStepHandler.Create(Fn: TStepHandlerFunc);
begin
  inherited Create;

  FFn := Fn;
end;

function TFunctionStepHandler.Handle(Task: TTask): Boolean;
begin
  Result := Self.FFn(Task);

end;

class function TFunctionStepHandler.FromFunc(Fn: TStepHandlerFunc
  ): TFunctionStepHandler;
begin
  Result := TFunctionStepHandler.Create(Fn);

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

constructor TPipeline.TStepInfo.Create(_StepID: Integer; _NumTasks: Integer;
  Handler: TStepHandler; Arguments: TObjectList);
begin
  inherited Create;

  FID := _StepID;
  FNumTasks := _NumTasks;
  StepHandler := Handler;
  FArguments := Arguments;

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
  ALoggerUnit.GetLogger.FMTDebugLn('Running Task %d', [Task.ID], 5);

  ALoggerUnit.GetLogger.FMTDebugLn('StepID: %d TaskID: %d', [Step.ID, Task.ID], 5);

  Result := Task.StepInfo.StepHandler.Handle(Task);

  ALoggerUnit.GetLogger.FMTDebugLn('wg.Done Task: %d Result: %s',
    [Task.ID, BoolToStr(Result, True)], 5);

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
    ALoggerUnit.FmtFatalLnIFFalse(False, 'Invalid Step', []);

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

  ALoggerUnit.GetLogger.FMTDebugLn(
    'Waiting for all jobs of Step %d to Finish',
    [Step.ID],
    5);
  Wg.Wait();
  ALoggerUnit.GetLogger.FMTDebugLn(
    'Step: %d All are Done',
    [Step.ID],
    5);

  ALoggerUnit.GetLogger.DebugLn('All jobs are Running');

  for i := 1 to Step.NumTasks do
  begin
    ALoggerUnit.GetLogger.FMTDebugLn(
      'Task %d is Done with Status: %s',
      [i, BoolToStr(Status[i], True)],
      5);
  end;

  SetLength(Status, 0);
  AllTasks.Free;
  Wg.Free;
  Result := True;
end;

constructor TPipeline.Create(constref _Name: AnsiString; _Config: TPipelineConfig);
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
  ThreadPool.Free;
  Config.Free;

  inherited Destroy;
end;

procedure TPipeline.AddNewStep(Handler: TStepHandler; NumTasks: Integer;
  Arguments: TObjectList);
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
  StepID := Integer(Pointer(SysArgs[2]));
  if StepID <> -1 then
  begin
    FromStepID := StepID;
    ToStepID := StepID;

  end;

  for ID := FromStepID to ToStepID do
    Result := ThePipeline.RunStep(ID);

  ALoggerUnit.GetLogger.FMTDebugLn('Result: %s', [BoolToStr(Result, True)], 5);

  wg.Done(1);

end;

function TPipeline.Run(StepID: Integer): Boolean;
var
  SysArgs: TObjectList;
  wg: TWaitGroup;
begin
  // TODO: Change SysArgs from TOjectList to TPointerList.
  SysArgs := TObjectList.Create;
  SysArgs.Add(Self);
  wg := TWaitGroup.Create;
  SysArgs.Add(wg);
  SysArgs.Add(TObject(Pointer(StepID)));

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
  ALoggerUnit.GetLogger.FMTDebugLn('%d StepIndex: %d', [ThreadID, StepIndex], 5);

  Result := RunStep(Steps[StepIndex]);

  ALoggerUnit.GetLogger.FMTDebugLn(
    '%d Result: %s',
    [ThreadID, BoolToStr(Result, True)],
    5);

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
