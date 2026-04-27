unit SyncUnit;

{$mode objfpc}{$H+}
{$COPERATORS ON}

interface

uses
  cthreads, Classes, SysUtils, Contnrs;

type
  { TMutex }
  TMutex = class(TObject)
  private
    CS: TRTLCriticalSection;

  public
    procedure Lock; inline;
    procedure Unlock; inline;

    constructor Create;
    destructor Destroy; override;
  end;

  { TRWMutex }

  TRWMutex = class(TMultiReadExclusiveWriteSynchronizer)
  public
    constructor Create;
    procedure RLock; inline;
    procedure RUnlock;
    procedure WLock;
    procedure WUnlock;

  end;

  { TSemaphore }
  { Cross-platform counting semaphore using RTLEvent
    Works on macOS (which doesn't support sem_init) }

  TSemaphore = class(TObject)
  private
    Mutex: TRTLCriticalSection;
    Event: PRTLEvent;
    Counter: Integer;

    function GetValue: Integer;

    procedure Inc;
    procedure Dec;

  public
    property Value: Integer read GetValue;

    constructor Create(const v: Integer);
    destructor Destroy; override;
    procedure Inc(const Delta: Integer);
    procedure Dec(const Delta: Integer);

  end;

  { TWaitGroup }

  TWaitGroup = class(TObject)
  private
    Mutex: TMutex;
    Value: Integer;
    BlockQueue: Contnrs.TQueue;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(k: Integer);
    procedure Done(k: Integer);
    function GetValue: Integer;

    procedure Wait;
  end;

  { TAtomic }

  generic TAtomic<T> = class(TObject)
  public
  type
    TApplyFunc = function(v: T): T;
  private
    FValue: T;
    Mutex: TMutex;

  public
    property Value: T read FValue;

    constructor Create(InitValue: T);
    destructor Destroy; override;

    function Apply(Func: TApplyFunc): T;

  end;

implementation

uses
  ALoggerUnit;

  { TAtomic }

constructor TAtomic.Create(InitValue: T);
begin
  inherited Create;

  Mutex := TMutex.Create;
  FValue := InitValue;
end;

destructor TAtomic.Destroy;
begin
  Mutex.Free;

  inherited Destroy;
end;

function TAtomic.Apply(Func: TApplyFunc): T;
begin
  Mutex.Lock;

  Result := Func(Value);
  FValue := Result;

  Mutex.Unlock;

end;

{ TRWMutex }

constructor TRWMutex.Create;
begin
  inherited Create;
end;

procedure TRWMutex.RLock;
begin
  inherited BeginRead;

end;

procedure TRWMutex.RUnlock;
begin
  inherited EndRead;

end;

procedure TRWMutex.WLock;
begin
  inherited BeginWrite;

end;

procedure TRWMutex.WUnlock;
begin
  inherited EndWrite;

end;

{ TWaitGroup }

constructor TWaitGroup.Create;
begin
  inherited Create;

  Value := 0;
  Mutex := TMutex.Create;
  BlockQueue := TQueue.Create;
end;

destructor TWaitGroup.Destroy;
begin
  Mutex.Free;
  BlockQueue.Free;

  inherited Destroy;
end;

procedure TWaitGroup.Add(k: Integer);
begin
  Mutex.Lock;

  Value += k;

  Mutex.Unlock;
end;

procedure TWaitGroup.Done(k: Integer);
var
  i: Integer;
begin
  Mutex.Lock;

  Value -= k;

  if Value < 0 then
    ALoggerUnit.GetLogger.FmtFatalLn('Value(%d) < 0', [Value]);

  if Value = 0 then
    for i := 1 to BlockQueue.Count do
    begin
      RTLEventSetEvent(PRTLEvent(BlockQueue.Pop));
    end;

  Mutex.Unlock;

end;

function TWaitGroup.GetValue: Integer;
begin
  Result := Value;
end;

procedure TWaitGroup.Wait;
var
  aWait: Boolean;
  anEvent: PRTLEvent;
begin
  Mutex.Lock;

  aWait := False;
  if 0 < Value then
  begin
    anEvent := RTLEventCreate;
    BlockQueue.Push(anEvent);
    aWait := True;

  end;

  Mutex.Unlock;

  if aWait then
  begin
    RTLeventWaitFor(anEvent);
    RTLEventDestroy(anEvent);

  end;

end;

{ TSemaphore }

function TSemaphore.GetValue: Integer;
begin
  EnterCriticalSection(Mutex);
  Result := Counter;
  LeaveCriticalSection(Mutex);
end;

constructor TSemaphore.Create(const v: Integer);
begin
  inherited Create;

  InitCriticalSection(Mutex);
  Event := RTLEventCreate;
  Counter := v;
  
  // If counter > 0, set event to signaled state
  if Counter > 0 then
    RTLEventSetEvent(Event);
end;

destructor TSemaphore.Destroy;
begin
  RTLEventDestroy(Event);
  DoneCriticalSection(Mutex);

  inherited Destroy;
end;

procedure TSemaphore.Inc;
begin
  EnterCriticalSection(Mutex);
  
  System.Inc(Counter);
  
  // Signal waiting threads
  if Counter > 0 then
    RTLEventSetEvent(Event);
    
  LeaveCriticalSection(Mutex);
end;

procedure TSemaphore.Dec;
var
  NeedWait: Boolean;
begin
  NeedWait := False;
  
  EnterCriticalSection(Mutex);
  
  // If counter is 0, we need to wait
  while Counter <= 0 do
  begin
    NeedWait := True;
    RTLEventResetEvent(Event);
    LeaveCriticalSection(Mutex);
    
    // Wait outside the mutex to avoid deadlock
    RTLEventWaitFor(Event);
    
    // Re-acquire mutex after waking up
    EnterCriticalSection(Mutex);
  end;
  
  // Decrement counter
  System.Dec(Counter);
  
  // If counter is still > 0, keep event signaled for other waiters
  if Counter <= 0 then
    RTLEventResetEvent(Event);
    
  LeaveCriticalSection(Mutex);
end;

procedure TSemaphore.Inc(const Delta: Integer);
var
  i: Integer;
begin
  for i := 1 to Delta do
    Self.Inc;

end;

procedure TSemaphore.Dec(const Delta: Integer);
var
  i: Integer;

begin
  for i := 1 to Delta do
    Self.Dec;

end;

{ TMutex }

procedure TMutex.Lock;
begin
  EnterCriticalSection(CS);

end;

procedure TMutex.Unlock;
begin
  LeaveCriticalSection(CS);

end;

constructor TMutex.Create;
begin
  inherited;

  InitCriticalSection(CS);

end;

destructor TMutex.Destroy;
begin
  DoneCriticalSection(CS);

  inherited Destroy;
end;

end.
