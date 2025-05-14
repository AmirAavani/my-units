unit ThreadPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, QueueUnit, SyncUnit, Generics.Collections;

type
  { TGenericThreadPool }

  generic TGenericThreadPool<TArguments>  = class(TObject)
  public type
    TThreadFunctionPtr = function (var Args: TArguments): Boolean;

    { TFuncArgResultArguments }

    TFuncArgResultArguments = record
      Arguments: TArguments;
      FuncPtr: TThreadFunctionPtr;
      PResult: PBoolean;
    end;

  private type
    TRequestsQueue = specialize TGenericBlockingQueue<TFuncArgResultArguments>;
    TThreads = specialize TList<TThread>;

    { TRunnerThread }

    TRunnerThread = class(TThread)
    private
      FParent: TGenericThreadPool;

    public
      constructor Create(Parent: TGenericThreadPool);
      destructor Destroy; override;

      procedure Execute; override;
    end;

  private
    RequestsQueue: TRequestsQueue;
    Done: Boolean;
    Wg: TWaitGroup;
    Threads: TThreads;

  public
    constructor Create(ThreadCount: Integer);
    destructor Destroy; override;

    procedure Run(
      F: TThreadFunctionPtr;
      Args: TArguments;
      OutputResult: PBoolean);
    procedure Wait;
    procedure Terminate;

  end;

implementation

{ TRunnerThread }

constructor TGenericThreadPool.TRunnerThread.Create(Parent: TGenericThreadPool);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FParent := Parent;

end;

destructor TGenericThreadPool.TRunnerThread.Destroy;
begin
  inherited Destroy;

end;

generic function EndOfThread<TArguments>(var Args: TArguments): Boolean;
begin
  Result := True;
end;

procedure TGenericThreadPool.TRunnerThread.Execute;
var
  Args: TFuncArgResultArguments;
  _Result: Boolean;

begin
  while True do
  begin
    FParent.RequestsQueue.Delete(Args);

    if Args.FuncPtr = nil then
    begin
      FParent.Wg.Done(1);
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

{ TGenericThreadPool }

constructor TGenericThreadPool.Create(ThreadCount: Integer);
var
  i: Integer;
  Thread: TThread;

begin
  inherited Create;

  RequestsQueue := TRequestsQueue.Create;
  Done := False;
  Threads := TThreads.Create;
  wg := TWaitGroup.Create;

  for i := 1 to ThreadCount do
    Threads.Add(TRunnerThread.Create(Self));
  for Thread in Threads do
    Thread.Start;

end;

generic function DoneThread<TArguments>(Arguments: TArguments): Boolean;
begin
  Result := True;

end;

destructor TGenericThreadPool.Destroy;
var
  Thread: TThread;
  Args: TArguments;

begin
  Self.Wait;
  for Thread in Threads do
  begin
    // TODO: Pointer to Generic Pointer is not implemented yet.
    // For now, we use the following code instead of
    //Self.Run(@(specialize DoneThread<TArguments>), Args, nil);

    Self.Run(nil, Args, nil);
  end;
  Self.Wait;

  Threads.Free;
  wg.Free;

  RequestsQueue.Free;

  inherited Destroy;

end;

procedure TGenericThreadPool.Run(F: TThreadFunctionPtr; Args: TArguments;
  OutputResult: PBoolean);
var
  Arg: TFuncArgResultArguments;

begin
  wg.Add(1);
  Arg.FuncPtr := F;
  Arg.Arguments := Args;
  Arg.PResult:= OutputResult;

  RequestsQueue.Insert(Arg);

end;

procedure TGenericThreadPool.Wait;
begin
  wg.Wait;

end;

procedure TGenericThreadPool.Terminate;
var
  Thread: TThread;

begin
  Self.Wait;

  for Thread in Self.Threads do
    Thread.Terminate;

end;

end.

