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
  public
    class function GetDefaultOptions: TWorkerPoolOptions;

  public type
    TWorkerFunction = function (Request: TRequest; Response: TResponse): Boolean;
    TCallbackProcedure = procedure (Request: TRequest; Response: TResponse);


  protected type
    TRequestInfo = record
      Request: TRequest;
      Response: TResponse;
      Callback: TCallbackProcedure;
    end;

    TPoolQueue = specialize TThreadSafeQueue<TRequestInfo>;
    { TBasePoolWorker }

    TBasePoolWorker = class(TThread)
    protected
      Parent: TWorkerPool;

      procedure Serve(Request: TRequest; Response: TResponse; Callback: TCallbackProcedure); virtual; abstract;

      procedure Execute; override;
    public
      constructor Create(_Parent: TWorkerPool);
      destructor Destroy; override;

    end;

    { TPoolWorkerFunc }

    TPoolWorkerFunc = class(TBasePoolWorker)
    protected
      FWorkerFunc: TWorkerFunction;

      procedure Serve(Request: TRequest; Response: TResponse; Callback: TCallbackProcedure); override;


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

    procedure ServeRequest(Request: TRequest; Response: TResponse; Callback: TCallbackProcedure = nil);

  end;

implementation
uses
  ALoggerUnit;

{ TWorkerPool.TPoolWorkerFunc }

procedure TWorkerPool.TPoolWorkerFunc.Serve(Request: TRequest;
  Response: TResponse; Callback: TCallbackProcedure);
begin
  if not FWorkerFunc(Request, Response) then
  begin
    FmtFatalLn('Failed', []);

  end;

  if Callback <> nil then
  begin
    Callback(Request, Response);

  end;

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
  ReqInfo: TRequestInfo;

begin
  ReqInfo := Default(TRequestInfo);

  while True do
  begin
    Parent.Queue.Delete(ReqInfo);

    if Parent.Queue.EndOfOperation then
      Break;

    Self.Serve(ReqInfo.Request, ReqInfo.Response, ReqInfo.Callback);
    if Parent.Options.FreeRequests then
      ReqInfo.Request.Free;

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

class function TWorkerPool.GetDefaultOptions: TWorkerPoolOptions;
begin
  Result.TimeOut := -1;
  Result.CreateNewThreadInNecessary := False;
  Result.NumberOfInitialThreads := 16;
  Result.MaxNumberOfThreads := 16;
  Result.FreeRequests := True;

end;

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

procedure TWorkerPool.ServeRequest(Request: TRequest; Response: TResponse;
  Callback: TCallbackProcedure);
var
  ReqInfo: TRequestInfo;

begin
  ReqInfo.Request := Request;
  ReqInfo.Response:= Response;
  ReqInfo.Callback:= Callback;

  Queue.Insert(ReqInfo);

end;

end.

