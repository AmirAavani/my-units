unit SchedularUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fptimer, SyncUnit, GenericCollectionUnit;

type

  { TObjectList }

  TObjectList = class(specialize TObjectCollection<TObject>)
  public
    function Add(Obj: TObject): TObjectList;

  end;

  TTaskProcedurePtr = procedure (Args: TObjectList);

  { TTaskDefinition }

  TTaskDefinition = class(TObject)
  public type
    TNotDoneOnDeadline = (ndodSkip = 1, ndodTerminate);
  private
    FArguments: TObjectList;
    FDeadline: UInt64;
    FIntervalInMSec: UInt64;
    FNotDoneOnDeadline: TNotDoneOnDeadline;
    FTaskProcedure: TTaskProcedurePtr;

  public
    property TaskProcedure: TTaskProcedurePtr read FTaskProcedure;
    property Arguments: TObjectList read FArguments;
    property IntervalInMSec: UInt64 read FIntervalInMSec;
    property Deadline: UInt64 read FDeadline;
    property NotDoneOnDeadline: TNotDoneOnDeadline read FNotDoneOnDeadline;

    constructor Create;
    destructor Destroy; override;

    function SetTaskProcedure(Task: TTaskProcedurePtr): TTaskDefinition;
    function SetArguments(Args: TObjectList): TTaskDefinition;
    function SetInterval(IntervalInMS: UInt64): TTaskDefinition;
    function SetDeadline(_Deadline: Uint64): TTaskDefinition;
    function SetNotDoneOnDeadline(WhatToDo: TNotDoneOnDeadline): TTaskDefinition;


  end;

  { TSchedular }

  TSchedular = class(TObject)
  private type
    TTaskExecutionStat = record
      LastStartTime: Int64;
      SkipCount: Integer;
      RunCount: Integer;
    end;

    { TTaskInfo }

    TTaskInfo = class(TObject)
      Definition: TTaskDefinition;
      Semaphore: TSemaphore;
      IsRunning: Boolean;
      Stat: TTaskExecutionStat;

    private
      Schedular: TSchedular;

    public
      constructor Create(_Schedular: TSchedular; d: TTaskDefinition);
      destructor Destroy; override;

    end;

    TAllTaskInfo = specialize TObjectCollection<TTaskInfo>;
    TThreads = specialize TList<TThread>;

  private
    AllTaskInfo: TAllTaskInfo;
    Threads: TThreads;
    Timer: TFPTimer;
    DoneWG: TWaitGroup;

    procedure OnTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTask(TaskDefinition: TTaskDefinition);
    procedure Stop;

    class function GetSchedular: TSchedular;

  end;

implementation
uses
  ALoggerUnit, DateTimeUtilUnit;

type
  TSchedularRunnerThread = class;

{ TObjectList }

function TObjectList.Add(Obj: TObject): TObjectList;
begin
  Result := Self;

  Result.Add(Obj);

end;

  { TSchedular.TTaskInfo }

constructor TSchedular.TTaskInfo.Create(_Schedular: TSchedular;
  d: TTaskDefinition);
begin
  inherited Create;

  Semaphore := TSemaphore.Create(0);
  Definition := d;
  IsRunning := False;
  FillChar(Stat, SizeOf(Stat), 0);
  Schedular := _Schedular;

end;

destructor TSchedular.TTaskInfo.Destroy;
begin
  Semaphore.Free;
  Definition.Free;

  inherited Destroy;
end;

{ TTaskDefinition }

constructor TTaskDefinition.Create;
begin
  inherited Create;

  FDeadline := 0;
end;

destructor TTaskDefinition.Destroy;
begin
  FArguments.Free;

  inherited Destroy;
end;

function TTaskDefinition.SetTaskProcedure(Task: TTaskProcedurePtr
  ): TTaskDefinition;
begin
  FTaskProcedure := Task;
  Result := Self;

end;

function TTaskDefinition.SetArguments(Args: TObjectList): TTaskDefinition;
begin
  FArguments := Args;
  Result := Self;

end;

function TTaskDefinition.SetInterval(IntervalInMS: UInt64): TTaskDefinition;
begin
  FIntervalInMSec := IntervalInMS;
  Result := Self;
end;

function TTaskDefinition.SetDeadline(_Deadline: Uint64): TTaskDefinition;
begin
  FDeadline := _Deadline;
  Result := Self;

end;

function TTaskDefinition.SetNotDoneOnDeadline(WhatToDo: TNotDoneOnDeadline
  ): TTaskDefinition;
begin
  FNotDoneOnDeadline := WhatToDo;
  Result := Self;

end;

{ TSchedular }

procedure TSchedular.OnTimer(Sender: TObject);
var
  i: Integer;
  NowTS: Int64;
  TaskInfo: TTaskInfo;

begin
  NowTS := CurrentTimestampInMS;

  for i := 0 to AllTaskInfo.Count - 1 do
  begin
    TaskInfo := AllTaskInfo[i];

    if (TaskInfo.Stat.LastStartTime + TaskInfo.Definition.IntervalInMSec < NowTs) then
    begin
      if TaskInfo.IsRunning then
      begin
        Inc(TaskInfo.Stat.SkipCount);
        Continue;

      end;

      Inc(TaskInfo.Stat.RunCount);
      TaskInfo.Stat.LastStartTime := NowTS;
      TaskInfo.Semaphore.Inc;
    end;
  end;
end;

type

  { TSchedularRunnerThread }

  TSchedularRunnerThread = class(TThread)
  private
    FDone: Boolean;
    TaskInfo: TSchedular.TTaskInfo;

  protected
    property Done: Boolean read FDone write FDone;

  public
    constructor Create(_TaskInfo: TSchedular.TTaskInfo);
    destructor Destroy; override;

    procedure Execute; override;

  end;

{ TSchedularRunnerThread }

constructor TSchedularRunnerThread.Create(_TaskInfo: TSchedular.TTaskInfo);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  TaskInfo := _TaskInfo;
  FDone := False;

end;

destructor TSchedularRunnerThread.Destroy;
begin

  inherited Destroy;
end;

procedure TSchedularRunnerThread.Execute;
begin
  Self.TaskInfo.Schedular.DoneWG.Add(1);

  while True do
  begin
    TaskInfo.Semaphore.Dec;
    TaskInfo.IsRunning := True;

    TaskInfo.Definition.TaskProcedure(TaskInfo.Definition.Arguments);

    TaskInfo.IsRunning := False;

    if Done then
      Break;

  end;

  Self.TaskInfo.Schedular.DoneWG.Done(1);

end;

constructor TSchedular.Create;
begin
  inherited Create;

  DoneWG := TWaitGroup.Create;
  AllTaskInfo := TAllTaskInfo.Create;
  Threads := TThreads.Create;

  Timer := TFPTimer.Create(nil);
  Timer.Interval := 100;
  Timer.UseTimerThread := True;
  Timer.OnTimer := @Self.OnTimer;
  Timer.Enabled := True;

end;

destructor TSchedular.Destroy;
var
  i: Integer;

begin
  Threads.Free;
  AllTaskInfo.Free;

  Timer.Free;
  DoneWG.Free;

  inherited Destroy;
end;

procedure TSchedular.AddTask(TaskDefinition: TTaskDefinition);
var
  Info: TSchedular.TTaskInfo;
  Thread: TThread;
begin
  Info := TTaskInfo.Create(Self, TaskDefinition);

  AllTaskInfo.Add(Info);
  Thread := TSchedularRunnerThread.Create(Info);
  Threads.Add(Thread);
  Thread.Start;

end;

procedure TSchedular.Stop;
var
  i: Integer;

begin
  Timer.Enabled := False;

  for i := 0 to Threads.Count - 1 do
  begin
    AllTaskInfo[i].Semaphore.Inc;
    (Threads[i] as TSchedularRunnerThread).Done := True;

  end;

  DoneWG.Wait;
  Threads.Clear;
end;

var
  Schedular: TSchedular;

class function TSchedular.GetSchedular: TSchedular;
begin
  Result := Schedular;

end;

initialization
  Schedular := TSchedular.Create;

finalization
  Schedular.Free;

end.

