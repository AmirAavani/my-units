unit WorkerPoolUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, QueueUnit, PairUnit, GenericCollectionUnit, ProtoHelperUnit,
  SourcerUnit, Generics.Collections, SyncUnit, PriorityQueueUnit;

type
  TSchedularPolicy = (
    spLargestTimestamp = 0,
    spSmallestTimestamp = 1,
    spLargestWorkerID = 2,
    spSmallestWorkerID = 3
  );

  TSchedularOptions = record
    SchedularPolicy: TSchedularPolicy;  // spLargestTimestamp, etc.
    MaxUnservedFromSource: Integer; // 1024
  end;

  { TWorkerPoolOptions }

  TWorkerPoolOptions = record
    TimeOut: Integer; // Not Implmented Yet.
    CreateNewThreadInNecessary: Boolean; // Not Implemented Yet.
    NumberOfInitialThreads: Integer; // Default value is 16.
    MaxNumberOfThreads: Integer; // Default value is 16.
    FreeRequests: Boolean; // Default value is True.
    NumberOfSourceThread: Integer; // Not Implemented Yet. Default is 1.
    SchedularOptions: TSchedularOptions;

  end;

  { TWorkerPool }

  generic TWorkerPool<TData> = class(TObject)
  public
    class function GetDefaultOptions: TWorkerPoolOptions;

  public type
    TBaseSourcerClass = specialize TBaseSourcer<TData>;
    TWorkerFunction = function (Request: TData): TData;

  protected type
    TPriorityFunction = function(NextWorkerID: Integer): Integer;

    { TSchedular }

    TSchedular = class(TObject)
    private
    protected type
      TStatus = (ssSuccess = 0, ssAllDone = 1);
      TQueue = specialize TPQueue<TData, Integer>;
      TRequest2WorkerID = specialize TMap<Pointer, Integer>;
      TIntList = specialize TCollection<Integer>;

    protected
      Options: TSchedularOptions;
      RequestCount: TIntList;
      Request2WorkerID: TRequest2WorkerID;
      Wg: TWaitGroup;
      Mutex: TMutex;
      PQueue: TQueue;
      PriorityFunction: TPriorityFunction;
      wp: TWorkerPool;

    public
      constructor Create(_Options: TSchedularOptions; _wp: TWorkerPool);
      destructor Destroy; override;

      procedure Add(Data: TData; NextWorkerID: Integer);
      function Delete(Data: TData; var NextWorkerID: Integer): TStatus;

    end;

    { TBasePoolWorker }

    TBasePoolWorker = class(TObject)
    private
      FWorkerID: Integer;
      function GetWorkerID: Integer; inline;

    private
      property WorkerID: Integer read GetWorkerID;

    protected
      WorkerPool: TWorkerPool;

      procedure AddToSchedular(MyResponse: TData); virtual;
      procedure AddToSchedular(MyResponse: TData; NextWorkerID: Integer); virtual;
      function Serve(Request: TData): TData; virtual; abstract;

    public
      constructor Create(_Parent: TWorkerPool);
      destructor Destroy; override;

    end;

    TMutexes = specialize TObjectCollection<TMutex>;
    TWorkerCollection = specialize TObjectCollection<TBasePoolWorker>;
    TWorkerIDMap = specialize TMap<Pointer, Integer>;

    { TBasePoolThread }

    TBasePoolThread = class(TThread)
    protected
      WorkerPool: TWorkerPool;

      procedure Execute; override;
    public
      constructor Create(_WorkerPool: TWorkerPool);
      destructor Destroy; override;

    end;


    (*

    { TPoolWorkerByFunction }

    TPoolWorkerByFunction = class(TBasePoolWorker)
    protected
      FWorkerFunction: TWorkerFunction;

    public
      constructor Create(WorkerFunc: TWorkerFunction);

    end;

    { TSourceReaderWorker }

    TSourceReaderWorker = class(TBasePoolWorker)
    protected
      FSource: TBaseSourcerClass;

      function Serve(Request: TData): TData; override;
    public
      constructor Create(_Parent: TWorkerPool; Source: TBaseSourcerClass);
      destructor Destroy; override;

    end;
    TThreadCollection = specialize TObjectCollection<TBasePoolThread>;
  *)
  protected
    Schedular: TSchedular;
    FOptions: TWorkerPoolOptions;
    FSource: TBaseSourcerClass;

    Workers: TWorkerCollection;
    Worker2IDMap: TWorkerIDMap;

    procedure DoCreateWithOption(Options: TWorkerPoolOptions);
    function GetWorkerByID(WorkerID: Integer): TBasePoolWorker;

    function Serve(Index: Integer; Request: TData): TData;

  public
    property Options: TWorkerPoolOptions read FOptions;
    constructor CreateWithOption(_Options: TWorkerPoolOptions);
    constructor CreateWithDefaultOptions;
    destructor Destroy; override;

    procedure SetSource(ASource: TBaseSourcerClass);
    procedure AddWorker(AWorker: TBasePoolWorker);
    procedure Start; virtual;

  end;

implementation
uses
  DateUtils, ALoggerUnit;

{ TWorkerPool.TSchedular }

constructor TWorkerPool.TSchedular.Create(_Options: TSchedularOptions;
  _wp: TWorkerPool);
const
  PriorityFunctions: array [Low(TSchedularPolicy)..High(TSchedularPolicy)] of TWorkerPool.TPriorityFunction =
    (
    nil, nil, nil, nil {
      @PFunctionFirstWorkerFirst,
      @PFunctionFirstWorkerLast,
      @PFunctionFirstTaskFirst,
      @PFunctionFirstTaskLast
      }
    );
begin
  inherited Create;

  Options:= _Options;
  PQueue := TQueue.Create;
  PriorityFunction := PriorityFunctions[Options.SchedularPolicy];
  wp := _wp;
  wg := TWaitGroup.Create;
  Mutex := TMutex.Create;
  RequestCount := TIntList.Create;
end;

destructor TWorkerPool.TSchedular.Destroy;
begin
  PQueue.Free;
  wg.Free;
  Mutex.Free;

  inherited Destroy;
end;

procedure TWorkerPool.TSchedular.Add(Data: TData; NextWorkerID: Integer);
begin
  Mutex.Lock;

  wg.Add(1);
  PQueue.Insert(Data, PriorityFunction(NextWorkerID));
  Request2WorkerID[Data] := NextWorkerID;

  while NextWorkerID <= RequestCount.Count do
    RequestCount.Add(0);
  RequestCount[NextWorkerID] := RequestCount[NextWorkerID] + 1;

  Mutex.Unlock;

end;

function TWorkerPool.TSchedular.Delete(Data: TData; var NextWorkerID: Integer
  ): TStatus;
begin
  Mutex.Lock;

  wg.Done(1);
  Result := ssSuccess;
  Data := PQueue.Delete;
  RequestCount[NextWorkerID] := RequestCount[NextWorkerID] - 1;
  Request2WorkerID.Delete(Pointer(Data), False);

  Mutex.Unlock;

end;

(*
{ TWorkerPool.TPoolWorkerByFunction }

constructor TWorkerPool.TPoolWorkerByFunction.Create(WorkerFunc: TWorkerFunction
  );
begin

end;
*)
{ TWorkerPool.TBasePoolWorker }

function TWorkerPool.TBasePoolWorker.GetWorkerID: Integer;
begin
  if FWorkerID = -1 then
    FWorkerID := WorkerPool.Worker2IDMap[Self];

  Result := FWorkerID;
end;

procedure TWorkerPool.TBasePoolWorker.AddToSchedular(MyResponse: TData);
begin
  AddToSchedular(MyResponse, WorkerID + 1);

end;

procedure TWorkerPool.TBasePoolWorker.AddToSchedular(MyResponse: TData;
  NextWorkerID: Integer);
begin
  WorkerPool.Schedular.Add(MyResponse, NextWorkerID);

end;

constructor TWorkerPool.TBasePoolWorker.Create(_Parent: TWorkerPool);
begin
  inherited Create;

  WorkerPool := _Parent;
  FWorkerID := -1;

end;

destructor TWorkerPool.TBasePoolWorker.Destroy;
begin

  inherited Destroy;
end;

(*
{ TWorkerPool.TSourceReaderWorker }

function TWorkerPool.TSourceReaderWorker.Serve(Request: TData): TData;
begin
  // Do nothing

end;

{
procedure TWorkerPool.TSourceReaderWorker.Execute;
var
  ReqInfo: TData;

begin
  ReqInfo := Default(TRequest);

  while not FSource.Done do
  begin
    ReqInfo := FSource.GetNext;
    Parent.Queue.Insert(ReqInfo);

    if Parent.Queue.EndOfOperation then
      Break;

    Self.Serve(ReqInfo.Request, ReqInfo.Response, ReqInfo.Callback);
    if Parent.Options.FreeRequests then
      ReqInfo.Request.Free;

  end;

end;
}

constructor TWorkerPool.TSourceReaderWorker.Create(_Parent: TWorkerPool;
  Source: TBaseSourcerClass);
begin
  inherited Create(_Parent);

  FSource := Source;
end;

destructor TWorkerPool.TSourceReaderWorker.Destroy;
begin
  FSource.Free;

  inherited Destroy;
end;

*)

{ TWorkerPool.TBasePoolThread }

procedure TWorkerPool.TBasePoolThread.Execute;
var
  Request: TData;
  Response: TData;
  Index: Integer;
  Status: TSchedular.TStatus;

begin
  Request := Default(TData);
  Index := -1;

  while True do
  begin
    Status := WorkerPool.Schedular.Delete(Request, Index);
    if Status = ssAllDone then
      Break;

    Response := WorkerPool.Serve(Index, Request);
    WorkerPool.Schedular.Add(Response, Index + 1);

    if WorkerPool.Options.FreeRequests then
      Request.Free;

  end;

end;

constructor TWorkerPool.TBasePoolThread.Create(_WorkerPool: TWorkerPool);
begin
  inherited Create(True);

  WorkerPool := _WorkerPool;

end;

destructor TWorkerPool.TBasePoolThread.Destroy;
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
  Result.NumberOfSourceThread := 1;
  Result.SchedularOptions.SchedularPolicy := spLargestTimestamp;
  Result.SchedularOptions.MaxUnservedFromSource := 1024;

end;

function TWorkerPool.Serve(Index: Integer; Request: TData): TData;
begin
  Result := Self.Workers[Index].Serve(Request);

end;

function PFunctionFirstWorkerFirst(NextWorkerID: Integer): Integer;
begin
  Result := NextWorkerID;

end;

function PFunctionFirstWorkerLast(NextWorkerID: Integer): Integer;
begin
  Result := -NextWorkerID;

end;

function PFunctionFirstTaskFirst(NextWorkerID: Integer): Integer;
begin
  Result := -DateTimeToUnix(Now);

end;

function PFunctionFirstTaskLast(NextWorkerID: Integer): Integer;
begin
  Result := DateTimeToUnix(Now);

end;

procedure TWorkerPool.DoCreateWithOption(Options: TWorkerPoolOptions);


var
  i: Integer;
  Thread: TBasePoolThread;

begin
  FOptions := Options;
  Schedular :=
    TSchedular.Create(Options.SchedularOptions, Self);

//  FThreads := TThreadCollection.Create;
  Workers := TWorkerCollection.Create;
  Worker2IDMap := TWorkerIDMap.Create;

  for i := 1 to FOptions.NumberOfInitialThreads do
  begin
    Thread := TBasePoolThread.Create(Self);
    Thread.FreeOnTerminate := True;
//    FThreads.Add(Thread);

  end;

end;

function TWorkerPool.GetWorkerByID(WorkerID: Integer): TBasePoolWorker;
begin
  Result := Workers[WorkerID];

end;

constructor TWorkerPool.CreateWithOption(_Options: TWorkerPoolOptions);
begin
  inherited Create;

  DoCreateWithOption(_Options);

end;

constructor TWorkerPool.CreateWithDefaultOptions;
begin
  inherited Create;

  DoCreateWithOption(GetDefaultOptions);

end;

destructor TWorkerPool.Destroy;
begin
  Schedular.Free;

  Workers.Free;
  Worker2IDMap.Free;

  inherited Destroy;

end;

procedure TWorkerPool.SetSource(ASource: TBaseSourcerClass);
begin
  FSource := ASource;

end;

procedure TWorkerPool.AddWorker(AWorker: TBasePoolWorker);
begin
//   Worker2IDMap.Add(AWorker, Workers.Count);
  Workers.Add(AWorker);

end;

{
procedure TWorkerPool.ServeRequest(Request: TData);
var
  Request: TData;

begin
  ReqInfo.Request := Request;
  // ReqInfo.Response:= Response;
  // ReqInfo.Callback:= Callback;

  Queue.Insert(ReqInfo);

end;
}

procedure TWorkerPool.Start;
var
  Thread: TBasePoolThread;

begin
  if FSource <> nil then

  // FSource.GetNext;
  //for Thread in FThreads do
  //  Thread.Start;

end;

end.

