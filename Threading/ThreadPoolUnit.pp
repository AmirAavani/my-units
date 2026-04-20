unit ThreadPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, QueueUnit, SyncUnit, Generics.Collections;

type
  { TGenericThreadPool }

  generic TGenericThreadPool<TArguments> = class(TObject)
  public type
    TThreadFunctionPtr = function (Args: TArguments): Boolean;

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
    Wg: TWaitGroup;
    Threads: TThreads;
    FCancelled: Boolean;

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
  FreeOnTerminate := False;

  FParent := Parent;

end;

destructor TGenericThreadPool.TRunnerThread.Destroy;
begin
  inherited Destroy;

end;

procedure TGenericThreadPool.TRunnerThread.Execute;
var
  Args: TFuncArgResultArguments;
  _Result: Boolean;

begin
  while not Terminated do
  begin
    FParent.RequestsQueue.Delete(Args);

    if Args.FuncPtr = nil then
    begin
      FParent.Wg.Done(1);
      Break;
    end;

    if FParent.FCancelled then
    begin
      FParent.Wg.Done(1);
      Continue;
    end;

    try
      try
        _Result := Args.FuncPtr(Args.Arguments);
        if Args.PResult <> nil then
        begin
          Args.PResult^ := _Result;

        end;
      except
        // Handle worker exceptions to prevent WaitGroup deadlock.
        if Args.PResult <> nil then
          Args.PResult^ := False;
      end;
    finally
      FParent.Wg.Done(1);
    end;

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
  Threads := TThreads.Create;
  wg := TWaitGroup.Create;
  FCancelled := False;

  for i := 1 to ThreadCount do
    Threads.Add(TRunnerThread.Create(Self));
  for Thread in Threads do
    Thread.Start;

end;

destructor TGenericThreadPool.Destroy;
var
  Thread: TThread;
  Args: TArguments;

begin
  Self.Wait;

  Args := Default(TArguments);
  for Thread in Threads do
  begin
    // Signal threads to exit.
    Self.Run(nil, Args, nil);
  end;

  Self.Wait;

  for Thread in Threads do
  begin
    Thread.WaitFor;
    Thread.Free;
  end;

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
  if FCancelled then
    Exit;

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
  Args: TArguments;

begin
  FCancelled := True;
  Args := Default(TArguments);

  for Thread in Self.Threads do
  begin
    Thread.Terminate;
    // Wake up threads blocked on queue.
    Self.Run(nil, Args, nil);
  end;

end;

end.

