unit WorkerPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ThreadSafeQueueUnit, PairUnit, GenericCollectionUnit;

type
  TRequest = Pointer;
  TResponse = Pointer;
  TWorkerFunction = function (Request: TRequest; Response: TResponse): Boolean;

  TWorkerPoolOptions = record
    TimeOut: Integer; // Not Implmented Yet.
    CreateNewThreadInNecessary: Boolean; // Not Implemented Yet.
    NumberOfInitialThreads: Integer;
    MaxNumberOfThreads: Integer;

  end;

  { TWorkerPool }

  TWorkerPool = class(TObject)
  protected type
    TRequestResponsePair = specialize TPair<TRequest, TResponse>;
    TPoolQueue = specialize TThreadSafeQueue<TRequestResponsePair>;

    { TBasePoolWorker }

    TBasePoolWorker = class(TThread)
    protected
      MyQueue: TPoolQueue;

      procedure Serve(Request: TRequest; Response: TResponse); virtual; abstract;

      procedure Execute; override;
    public
      constructor Create(_Queue: TPoolQueue);
      destructor Destroy; override;

    end;

    { TPoolWorkerFunc }

    TPoolWorkerFunc = class(TBasePoolWorker)
    protected
      FWorkerFunc: TWorkerFunction;

      procedure Serve(Request: TRequest; Response: TResponse); override;


    public
      constructor Create(WorkerFunc: TWorkerFunction; _Queue: TPoolQueue);
    end;

  protected
    FThreads: specialize TObjectCollection<TThread>;
    FOptions: TWorkerPoolOptions;
    Queue: TPoolQueue;

  public
    constructor CreateWithOption(WorkerFunc: TWorkerFunction; Options: TWorkerPoolOptions);
    constructor CreateWithDefaultOptions(WorkerFunc: TWorkerFunction);
    destructor Destroy; override;

    procedure ServeRequest(Request: TRequest; Response: TResponse);

  end;

function GetDefaultOptions: TWorkerPoolOptions;

implementation
uses
  ALoggerUnit;

function GetDefaultOptions: TWorkerPoolOptions;
begin
  Result.TimeOut := -1;
  Result.CreateNewThreadInNecessary := False;
  Result.NumberOfInitialThreads := 16;
  Result.MaxNumberOfThreads := 16;

end;

{ TWorkerPool.TPoolWorkerFunc }

procedure TWorkerPool.TPoolWorkerFunc.Serve(Request: TRequest;
  Response: TResponse);
begin
  if not FWorkerFunc(Request, Response) then
    FmtFatalLn('Failed', []);
end;

constructor TWorkerPool.TPoolWorkerFunc.Create(WorkerFunc: TWorkerFunction;
  _Queue: TPoolQueue);
begin
  inherited Create(_Queue);

  FWorkerFunc := WorkerFunc;


end;

{ TWorkerPool.TBasePoolWorker }

procedure TWorkerPool.TBasePoolWorker.Execute;
var
  Pair: TRequestResponsePair;

begin
  Pair := Default(TRequestResponsePair);

  while True do
  begin
    Self.MyQueue.Delete(Pair);
    Self.Serve(Pair.First, Pair.Second);

  end;
end;

constructor TWorkerPool.TBasePoolWorker.Create(_Queue: TPoolQueue);
begin
  inherited Create(True);

  MyQueue := _Queue;

end;

destructor TWorkerPool.TBasePoolWorker.Destroy;
begin
  inherited Destroy;
end;

{ TWorkerPool }

constructor TWorkerPool.CreateWithOption(WorkerFunc: TWorkerFunction;
  Options: TWorkerPoolOptions);
var
  i: Integer;
  Thread: TPoolWorkerFunc;

begin
  inherited Create;

  FOptions := Options;
  Queue := TPoolQueue.Create;
  FThreads := (specialize TObjectCollection<TThread>).Create;

  for i := 0 to FOptions.NumberOfInitialThreads do
  begin
    Thread := TPoolWorkerFunc.Create(WorkerFunc, Queue);
    FThreads.Add(Thread);
    Thread.Start;

  end;
end;

constructor TWorkerPool.CreateWithDefaultOptions(WorkerFunc: TWorkerFunction);
begin
  CreateWithOption(WorkerFunc, GetDefaultOptions);

end;

destructor TWorkerPool.Destroy;
begin
  Queue.Free;

  inherited Destroy;
end;

procedure TWorkerPool.ServeRequest(Request: TRequest; Response: TResponse
  );
var
  Pair: TRequestResponsePair;

begin
  Pair.First := Request;
  Pair.Second:= Response;

  Queue.Insert(Pair);

end;

end.

