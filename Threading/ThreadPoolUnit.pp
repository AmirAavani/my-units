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

    // Check Terminated flag again after unblocking
    if Terminated then
      Break;

    // nil function pointer = shutdown signal
    if Args.FuncPtr = nil then
      Break;

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
        on E: Exception do
        begin
          // Handle worker exceptions to prevent WaitGroup deadlock
          WriteLn(Format('[Thread] Exception in worker: %s: %s', [E.ClassName, E.Message]));
          // Return False on exception if result pointer provided
          if Args.PResult <> nil then
            Args.PResult^ := False;
        end;
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

  RequestsQueue := TRequestsQueue.Create;
  Threads := TThreads.Create;
  wg := TWaitGroup.Create;
  FCancelled := False;

  // Create worker threads (suspended)
  for i := 1 to ThreadCount do
    Threads.Add(TRunnerThread.Create(Self));
    
  // Start all workers
  for Thread in Threads do
    Thread.Start;

end;

destructor TGenericThreadPool.Destroy;
var
  Thread: TThread;
  Arg: TFuncArgResultArguments;
  i: Integer;

begin
  WriteLn('[Destroy] Waiting for pending tasks...');
  Flush(Output);
  // Wait for all pending tasks to complete
  Self.Wait;
  
  WriteLn('[Destroy] Shutting down queue...');
  Flush(Output);
  // Shutdown the queue first - this wakes all blocked threads
  RequestsQueue.Shutdown;
  
  WriteLn('[Destroy] Setting Terminated flag on all threads...');
  Flush(Output);
  // Set Terminated flag on all threads
  for Thread in Threads do
    Thread.Terminate;
  
  WriteLn('[Destroy] Waiting for threads to terminate...');
  Flush(Output);
  // Wait for threads to exit and free them
  for i := 0 to Threads.Count - 1 do
  begin
    WriteLn(Format('[Destroy]   WaitFor thread %d/%d...', [i+1, Threads.Count]));
    Flush(Output);
    Threads[i].WaitFor;  // Block until thread terminates
    WriteLn(Format('[Destroy]   Thread %d terminated, freeing...', [i+1]));
    Flush(Output);
    Threads[i].Free;
  end;

  WriteLn('[Destroy] Cleaning up resources...');
  Flush(Output);
  Threads.Free;
  wg.Free;  
  RequestsQueue.Free;

  WriteLn('[Destroy] Done!');
  Flush(Output);
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

end;

end.
