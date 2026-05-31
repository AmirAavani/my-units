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
  MyThreadID: PtrUInt;

begin
  // FIRST THING: print that we entered Execute
  WriteLn('[Thread] Execute ENTERED');
  Flush(Output);
  
  MyThreadID := PtrUInt(ThreadID);
  
  WriteLn(Format('[Thread-%d] Execute method started', [MyThreadID]));
  Flush(Output);

  while not Terminated do
  begin    
    WriteLn(Format('[Thread-%d] About to call RequestsQueue.Delete...', [MyThreadID]));
    Flush(Output);

    // Block until task available in queue
    FParent.RequestsQueue.Delete(Args);
    
    WriteLn(Format('[Thread-%d] Got task: FuncPtr=%p, Args=%p, Terminated=%d, FCancelled=%d', 
      [MyThreadID, Pointer(@Args.FuncPtr), Pointer(Args.Arguments), Ord(Terminated), Ord(FParent.FCancelled)]));
    Flush(Output);

    // Check if queue is shutting down (after Delete returns)
    if FParent.RequestsQueue.IsShuttingDown then
      Break;

    // Check Terminated flag again after unblocking
    if Terminated then
      Break;

    // nil function pointer = shutdown signal
    if Args.FuncPtr = nil then
      Break;

    // Skip task if pool is shutting down
    if FParent.FCancelled then
    begin
      WriteLn(Format('[Thread-%d] Skipping task (FCancelled=true)', [MyThreadID]));
      Flush(Output);
      FParent.Wg.Done(1);
      Continue;
    end;

    // Execute worker function
    WriteLn(Format('[Thread-%d] Executing task with Args=%p...', [MyThreadID, Pointer(Args.Arguments)]));
    Flush(Output);

    _Result := Args.FuncPtr(Args.Arguments);
    
    WriteLn(Format('[Thread-%d] Task completed successfully (Args=%p)', [MyThreadID, Pointer(Args.Arguments)]));
    Flush(Output);
    if Args.PResult <> nil then
    begin
      Args.PResult^ := _Result;
    end;
    
    // Decrement wait group
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

  WriteLn(Format('[Create] Creating thread pool with %d threads...', [ThreadCount]));
  Flush(Output);

  RequestsQueue := TRequestsQueue.Create;
  Threads := TThreads.Create;
  wg := TWaitGroup.Create;
  FCancelled := False;

  // Create worker threads (suspended)
  for i := 1 to ThreadCount do
  begin
    WriteLn(Format('[Create] Creating thread %d/%d...', [i, ThreadCount]));
    Flush(Output);
    Threads.Add(TRunnerThread.Create(Self));
  end;
    
  WriteLn(Format('[Create] Starting %d threads...', [Threads.Count]));
  Flush(Output);
  
  // Start all workers
  for i := 0 to Threads.Count - 1 do
  begin
    WriteLn(Format('[Create] Starting thread %d (ID will be assigned)...', [i]));
    Flush(Output);
    Threads[i].Start;
    WriteLn(Format('[Create] Thread %d started', [i]));
    Flush(Output);
  end;

  WriteLn('[Create] Thread pool created and started');
  Flush(Output);
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
  // Shutdown the queue - this sets state and wakes threads
  RequestsQueue.Shutdown;
  
  WriteLn('[Destroy] Setting Terminated flag on all threads...');
  Flush(Output);
  // Set Terminated flag on all threads
  for Thread in Threads do
    Thread.Terminate;
  
  WriteLn('[Destroy] Waking up blocked threads...');
  Flush(Output);
  // Ensure each thread gets woken up by incrementing semaphore once per thread
  // The Shutdown() already did Inc(1000), but let's be explicit
  for i := 1 to Threads.Count do
    RequestsQueue.WakeOne;
  
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
  begin
    WriteLn(Format('[Run] Rejecting task (FCancelled=true, Args=%p)', [Pointer(Args)]));
    Flush(Output);
    Exit;
  end;

  // Increment wait group BEFORE queuing task
  wg.Add(1);
  
  // Package task data
  Arg.FuncPtr := F;
  Arg.Arguments := Args;
  Arg.PResult:= OutputResult;

  WriteLn(Format('[Run] Queuing task: FuncPtr=%p, Args=%p', [Pointer(@F), Pointer(Args)]));
  Flush(Output);
  
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
