unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pipeline.TypesUnit, ThreadSafeQueueUnit, GenericCollectionUnit;

type
  TTask = class;
  TStepHandler = function (Task: TTask): Boolean;
  TFilenames = TStringList;

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
    function RunStep(Step: TStepInfo): Boolean;

  function RunStep(StepIndex: Integer): Boolean;
  function RunFromStep(StepIndex: Integer): Boolean;
  public
    property Name: AnsiString read FName;

    constructor Create(_Name: AnsiString);
    destructor Destroy; override;

    procedure AddNewStep(Handler: TStepHandler; NumTasks: Integer);

    function Run(WaitToFinish: Boolean = True): Boolean;
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

    function FilterFilesModule(aPattern: AnsiString): TAnsiStringList;
    function FilterFilesDiv(aPattern: AnsiString): TAnsiStringList;
  end;

implementation
uses
  SyncUnit, StringUnit, ALoggerUnit, RunInAThreadUnit, ParameterManagerUnit,
  Pipeline.Utils;

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

function TTask.FilterFilesModule(aPattern: AnsiString): TAnsiStringList;
var
  Tmp: TAnsiStringList;

begin
  Tmp := ExpandPattern(aPattern);
  Result := Pipeline.Utils.FilterFilesModule(Tmp, Self.ID, Self.StepInfo.NumTasks);
  Tmp.Free;

end;

function TTask.FilterFilesDiv(aPattern: AnsiString): TAnsiStringList;
var
  Tmp: TAnsiStringList;

begin
  Tmp := ExpandPattern(aPattern);
  Result := Pipeline.Utils.FilterFilesDiv(Tmp, Self.ID, Self.StepInfo.NumTasks);
  Tmp.Free;

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
  FMTDebugLn('Running Task %d', [Task.ID]);

  FMTDebugLn('StepID: %d TaskID: %d', [Step.ID, Task.ID]);

  Result := Task.StepInfo.StepHandler(Task);

  FMTDebugLn('wg.Done Task: %d Result: %s', [Task.ID, BoolToStr(Result, True)]);

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

    RunInThread(@RunHandler, SysArgs, @Status[i]);

  end;

  FMTDebugLn('Waiting for all jobs of Step %d to Finish', [Step.ID]);
  Wg.Wait();
  FMTDebugLn('Step: %d All are Done', [Step.ID]);

  DebugLn('All jobs are Running');

  for i := 1 to Step.NumTasks do
  begin
    FMTDebugLn('Task %d is Done with Status: %s', [i, BoolToStr(Status[i], True)]);
  end;

  SetLength(Status, 0);
  AllTasks.Free;
  Wg.Free;
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

begin
  ThePipeline := TPipeline(SysArgs[0]);
  wg := TWaitGroup(SysArgs[1]);

  Result := False;
  StepID := GetRunTimeParameterManager.ValueByName['Pipeline.StepID'].AsIntegerOrDefault(-1);
  FromStepID := GetRunTimeParameterManager.ValueByName['Pipeline.FromStepID'].AsIntegerOrDefault(1);

  if StepID <> -1 then
    Result := ThePipeline.RunStep(StepID)
  else
    Result := ThePipeline.RunFromStep(FromStepID);

  FMTDebugLn('Result: %s', [BoolToStr(Result, True)]);

  wg.Done(1);

end;

function TPipeline.Run(WaitToFinish: Boolean): Boolean;
var
  SysArgs: TObjectList;
  wg: TWaitGroup;
  Done: Boolean;

begin
  SysArgs := TObjectList.Create;
  SysArgs.Add(Self);
  wg := TWaitGroup.Create;
  SysArgs.Add(wg);

  wg.Add(1);
  if WaitToFinish then
    RunInThread(@RunThePipeline, SysArgs, @Done)
  else
    RunInThread(@RunThePipeline, SysArgs, nil);

  if WaitToFinish then
    wg.Wait;

  Result := Done;
  wg.Free;
  SysArgs.Free;

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

