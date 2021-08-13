unit WorkerPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ThreadSafeQueueUnit, PairUnit, GenericCollectionUnit, ProtoHelperUnit;

type
  TWorkerPoolOptions = record
    TimeOut: Integer; // Not Implmented Yet.
    CreateNewThreadInNecessary: Boolean; // Not Implemented Yet.
    NumberOfInitialThreads: Integer; // Default value is 16.
    MaxNumberOfThreads: Integer; // Default value is 16.
    FreeRequests: Boolean; // Default value is True.

  end;

  { TWorkerPool }

  generic TWorkerPool<TRequest: TBaseMessage; TResponse: TBaseMessage> = class(TObject)
  public type
    TWorkerFunction = function (Request: TRequest; Response: TResponse): Boolean;


  protected type
    TRequestResponsePair = specialize TPair<TRequest, TResponse>;
    TPoolQueue = specialize TThreadSafeQueue<TRequestResponsePair>;
    { TBasePoolWorker }

    TBasePoolWorker = class(TThread)
    protected
      Parent: TWorkerPool;

      procedure Serve(Request: TRequest; Response: TResponse); virtual; abstract;

      procedure Execute; override;
    public
      constructor Create(_Parent: TWorkerPool);
      destructor Destroy; override;

    end;

    { TPoolWorkerFunc }

    TPoolWorkerFunc = class(TBasePoolWorker)
    protected
      FWorkerFunc: TWorkerFunction;

      procedure Serve(Request: TRequest; Response: TResponse); override;


    public
      constructor Create(_Parent: TWorkerPool; WorkerFunc: TWorkerFunction);
      destructor Destroy; override;
    end;

    TThreadCollection = specialize TObjectCollection<TBasePoolWorker>;

  protected
    FThreads: TThreadCollection;
    FOptions: TWorkerPoolOptions;
    Queue: TPoolQueue;

    procedure DoCreateWithOption(WorkerFunc: TWorkerFunction; Options: TWorkerPoolOptions);

  public
    property Options: TWorkerPoolOptions read FOptions;

    constructor CreateWithOption(WorkerFunc: TWorkerFunction; _Options: TWorkerPoolOptions);
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
  Result.FreeRequests := True;

end;

{ TWorkerPool.TPoolWorkerFunc }

procedure TWorkerPool.TPoolWorkerFunc.Serve(Request: TRequest;
  Response: TResponse);
begin
  if not FWorkerFunc(Request, Response) then
    FmtFatalLn('Failed', []);
end;

constructor TWorkerPool.TPoolWorkerFunc.Create(_Parent: TWorkerPool;
  WorkerFunc: TWorkerFunction);
begin
  inherited Create(_Parent);

  FWorkerFunc := WorkerFunc;

end;

destructor TWorkerPool.TPoolWorkerFunc.Destroy;
begin
  inherited Destroy;

end;

{ TWorkerPool.TBasePoolWorker }

procedure TWorkerPool.TBasePoolWorker.Execute;
var
  Pair: TRequestResponsePair;

begin
  Pair := Default(TRequestResponsePair);

  while True do
  begin
    Parent.Queue.Delete(Pair);

    if Parent.Queue.EndOfOperation then
      Break;

    Self.Serve(Pair.First, Pair.Second);
    if Parent.Options.FreeRequests then
      Pair.First.Free;

  end;

end;

constructor TWorkerPool.TBasePoolWorker.Create(_Parent: TWorkerPool);
begin
  inherited Create(True);

  Parent := _Parent;

end;

destructor TWorkerPool.TBasePoolWorker.Destroy;
begin

  inherited Destroy;
end;

{ TWorkerPool }

procedure TWorkerPool.DoCreateWithOption(WorkerFunc: TWorkerFunction;
  Options: TWorkerPoolOptions);
var
  i: Integer;
  Thread: TPoolWorkerFunc;

begin
  FOptions := Options;
  Queue := TPoolQueue.Create;
  FThreads := TThreadCollection.Create;

  Queue.EndOfOperation := False;
  for i := 0 to FOptions.NumberOfInitialThreads do
  begin
    Thread := TPoolWorkerFunc.Create(Self,WorkerFunc);
    Thread.FreeOnTerminate := True;
    FThreads.Add(Thread);
    Thread.Start;

  end;

end;

constructor TWorkerPool.CreateWithOption(WorkerFunc: TWorkerFunction;
  _Options: TWorkerPoolOptions);
begin
  inherited Create;

  DoCreateWithOption(WorkerFunc, Options);

end;

constructor TWorkerPool.CreateWithDefaultOptions(WorkerFunc: TWorkerFunction);
begin
  inherited Create;

  DoCreateWithOption(WorkerFunc, GetDefaultOptions);

end;

destructor TWorkerPool.Destroy;
begin
  Queue.EndOfOperation := True;
  Queue.Free;

  FThreads.Clear;
  FThreads.Free;

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

