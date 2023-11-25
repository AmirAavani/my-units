unit ThreadPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ThreadSafeQueueUnit, SyncUnit, GenericCollectionUnit;//,
 // PairUnit;

type
  TObjectList = specialize TObjectCollection<TObject>;
  TThreadFunctionPtr = function (Args: TObjectList): Boolean;

  { TFuncArgResultArgument }

  TFuncArgResultArgument = record
    IsValid: Boolean;
    Arguments: TObjectList;
    FuncPtr: TThreadFunctionPtr;
    PResult: PBoolean;
  end;

  { TThreadPool }

  TThreadPool = class(TObject)
  private type
    TRequestsQueue = specialize TThreadSafeQueue<TFuncArgResultArgument>;
    TThreads = specialize TCollection<TThread>;

  private
    RequestsQueue: TRequestsQueue;
    Done: Boolean;
    Wg: TWaitGroup;
    Threads: TThreads;

  public
    constructor Create(ThreadCount: Integer);
    destructor Destroy; override;

    procedure Run(F: TThreadFunctionPtr; Args: TObjectList; OutputResult: PBoolean);
    procedure Wait;
  end;

implementation
uses
  ALoggerUnit;

type

  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    FParent: TThreadPool;

  public
    constructor Create(Parent: TThreadPool);

    procedure Execute; override;
  end;

{ TRunnerThread }

constructor TRunnerThread.Create(Parent: TThreadPool);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FParent := Parent;

end;

procedure TRunnerThread.Execute;
var
  Args: TFuncArgResultArgument;
  _Result: Boolean;

begin
  while True do
  begin
    ALoggerUnit.GetLogger.FMTDebugLn('Before Delete', [], 5);

    FParent.RequestsQueue.Delete(Args);

    if not Args.IsValid then
    begin
      Break;
    end;

    _Result := Args.FuncPtr(Args.Arguments);
    if Args.PResult <> nil then
    begin
      Args.PResult^ := _Result;

    end;

    FParent.Wg.Done(1);

  end;
end;

{ TThreadPool }

constructor TThreadPool.Create(ThreadCount: Integer);
var
  i: Integer;

begin
  inherited Create;

  RequestsQueue := TRequestsQueue.Create;
  Done := False;
  Threads := TThreads.Create;
  wg := TWaitGroup.Create;

  for i := 1 to ThreadCount do
    Threads.Add(TRunnerThread.Create(Self));
  for i := 0 to ThreadCount - 1 do
    Threads[i].Start;

end;

destructor TThreadPool.Destroy;
begin
  Done := True;
  wg.Free;

  RequestsQueue.Free;
  Threads.Free;

  inherited Destroy;
end;

procedure TThreadPool.Run(F: TThreadFunctionPtr; Args: TObjectList;
  OutputResult: PBoolean);
var
  Arg: TFuncArgResultArgument;

begin
  wg.Add(1);
  Arg.IsValid := True;
  Arg.FuncPtr := F;
  Arg.Arguments := Args;
  Arg.PResult:= OutputResult;

  RequestsQueue.Insert(Arg);

end;

procedure TThreadPool.Wait;
begin
  wg.Wait;
end;

end.

