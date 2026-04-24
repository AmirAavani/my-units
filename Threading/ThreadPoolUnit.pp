unit ThreadPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, QueueUnit, SyncUnit, Generics.Collections;

type
  { TGenericThreadPool }
  { Generic thread pool implementation for parallel task execution.
    Thread-safe task queue with automatic worker thread management.
    
    Usage:
      Pool := TGenericThreadPool.Create(NumThreads);
      Pool.Run(@WorkerFunc, Data, @Result);
      Pool.Wait;  // Wait for all tasks to complete
      Pool.Free;
  }

  generic TGenericThreadPool<TArguments> = class(TObject)
  public type
    { Function pointer type for worker functions.
      Args: Input data for the worker
      Returns: Boolean indicating success/failure }
    TThreadFunctionPtr = function (Args: TArguments): Boolean;

    { TFuncArgResultArguments }
    { Internal structure to package task data for the queue }
    TFuncArgResultArguments = record
      Arguments: TArguments;        // Input data for worker function
      FuncPtr: TThreadFunctionPtr;  // Worker function (nil = shutdown signal)
      PResult: PBoolean;            // Optional output (can be nil)
    end;

  private type
    { Thread-safe blocking queue for task distribution }
    TRequestsQueue = specialize TGenericBlockingQueue<TFuncArgResultArguments>;
    { List of worker threads }
    TThreads = specialize TList<TThread>;

    { TRunnerThread }
    { Worker thread that pulls tasks from queue and executes them.
      Terminates when it receives a nil function pointer. }
    TRunnerThread = class(TThread)
    private
      FParent: TGenericThreadPool;  // Reference to owning pool

    public
      constructor Create(Parent: TGenericThreadPool);
      destructor Destroy; override;

      { Main worker loop - blocks on queue until task available }
      procedure Execute; override;
    end;

  private
    RequestsQueue: TRequestsQueue;  // Blocking queue for tasks
    Wg: TWaitGroup;                 // Wait group for tracking tasks
    Threads: TThreads;              // Pool of worker threads
    FCancelled: Boolean;            // Shutdown flag

  public
    { Create thread pool with specified number of workers }
    constructor Create(ThreadCount: Integer);
    
    { Clean shutdown: waits for tasks, signals workers to exit }
    destructor Destroy; override;

    { Submit task to thread pool
      F: Worker function to execute
      Args: Input data for worker
      OutputResult: Optional pointer to store result (can be nil) }
    procedure Run(
      F: TThreadFunctionPtr;
      Args: TArguments;
      OutputResult: PBoolean);
      
    { Block until all pending tasks complete }
    procedure Wait;
    
    { Cancel all pending tasks and signal threads to exit }
    procedure Terminate;

  end;

implementation

{ TRunnerThread }

constructor TGenericThreadPool.TRunnerThread.Create(Parent: TGenericThreadPool);
begin
  inherited Create(True);  // Create suspended
  FreeOnTerminate := False; // We manually free threads

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
    // Block until task available in queue
    FParent.RequestsQueue.Delete(Args);

    // nil function pointer = shutdown signal
    if Args.FuncPtr = nil then
    begin
      FParent.Wg.Done(1);
      Break;
    end;

    // Skip task if pool is shutting down
    if FParent.FCancelled then
    begin
      FParent.Wg.Done(1);
      Continue;
    end;

    try
      try
        // Execute worker function
        _Result := Args.FuncPtr(Args.Arguments);
        if Args.PResult <> nil then
        begin
          Args.PResult^ := _Result;

        end;
      except
        // Handle worker exceptions to prevent WaitGroup deadlock
        // Return False on exception if result pointer provided
        if Args.PResult <> nil then
          Args.PResult^ := False;
      end;
    finally
      // ALWAYS decrement wait group to prevent deadlock
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
  WriteLn('Create.1');
  RequestsQueue := TRequestsQueue.Create;
  WriteLn('Create.2');
  Threads := TThreads.Create;
  WriteLn('Create.3');
  wg := TWaitGroup.Create;
  WriteLn('Create.4');
  FCancelled := False;

  // Create worker threads (suspended)
  for i := 1 to ThreadCount do
    Threads.Add(TRunnerThread.Create(Self));
  WriteLn('Create.5');
    
  // Start all workers
  for Thread in Threads do
    Thread.Start;
  WriteLn('Create.6');

end;

destructor TGenericThreadPool.Destroy;
var
  Thread: TThread;
  Args: TArguments;

begin
  WriteLn(1);
  Flush(Output);
  // Wait for all pending tasks to complete
  Self.Wait;
  WriteLn(2);
  Flush(Output);

  // Send shutdown signal to each worker (nil function pointer)
  Args := Default(TArguments);
  for Thread in Threads do
  begin
    Self.Run(nil, Args, nil);
  end;

  WriteLn(5);
  Flush(Output);
  // Wait for threads to exit and free them
  for Thread in Threads do
  begin
    Thread.WaitFor;  // Block until thread terminates
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
  // Don't accept new tasks during shutdown
  if FCancelled then
    Exit;

  // Increment wait group BEFORE queuing task
  wg.Add(1);
  
  // Package task data
  Arg.FuncPtr := F;
  Arg.Arguments := Args;
  Arg.PResult:= OutputResult;

  // Add to queue - worker will pick it up
  RequestsQueue.Insert(Arg);

end;

procedure TGenericThreadPool.Wait;
begin
  // Block until wait group counter reaches zero
  // (all submitted tasks have called wg.Done)
  wg.Wait;

end;

procedure TGenericThreadPool.Terminate;
var
  Thread: TThread;
  Args: TArguments;

begin
  // Set flag to reject new tasks
  FCancelled := True;
  Args := Default(TArguments);

  for Thread in Self.Threads do
  begin
    // Set TThread.Terminated flag
    Thread.Terminate;
    
    // Wake up threads blocked on queue by sending dummy task
    // This allows them to check Terminated flag
    Self.Run(nil, Args, nil);
  end;

  // NOTE: Removed second Self.Wait() here - it causes hangs because threads
  // exit after processing their nil signal and won't process additional signals.
  // Thread.WaitFor() below is sufficient to wait for thread termination.

  WriteLn(5);
  Flush(Output);
