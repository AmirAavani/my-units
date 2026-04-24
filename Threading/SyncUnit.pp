unit SyncUnit;

{$mode objfpc}{$H+}
{$COPERATORS ON}
{$IFDEF DARWIN}
  {$DEFINE SYNC_MACOS}
{$ENDIF}

interface

uses
  cthreads, pthreads, Classes, SysUtils, Contnrs;

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
    procedure RLock; inline;
    procedure RUnlock;
    procedure WLock;
    procedure WUnlock;

  end;

  { TSemaphore }

  TSemaphore = class(TObject)
  private
    Sem: psem_t;

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
  ALoggerUnit, BaseUnix;

  { TAtomic }

constructor TAtomic.Create(InitValue: T);
begin
  inherited Create;

  Mutex := TMutex.Create;
  FValue := InitValue;
end;

destructor TAtomic.Destroy;
begin
  {$IFNDEF SYNC_MACOS}
  // On macOS, Mutex.Free can fail - skip it
  Mutex.Free;
  {$ENDIF}

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
  {$IFNDEF SYNC_MACOS}
  // On macOS, Mutex.Free can fail - skip it
  Mutex.Free;
  {$ENDIF}
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
  if sem_getvalue(Sem, @Result) <> 0 then
  begin
    Result := -MaxInt;
    WriteLn(Format('Failed in getting value, (err: %d)', [fpgeterrno]));
  end;

end;

constructor TSemaphore.Create(const v: Integer);
begin
  inherited Create;

  New(Sem);
  if sem_init(Sem, 0, v) <> 0 then
    ALoggerUnit.GetLogger.FmtFatalLn('Failed in sem_init, (err: %d)', [fpgeterrno]);
end;

destructor TSemaphore.Destroy;
begin
  {$IFNDEF SYNC_MACOS}
  // On macOS, sem_destroy can fail with ENOSYS (78) - Function not implemented
  // This is a known limitation of macOS's semaphore implementation
  if sem_destroy(Sem) <> 0 then
    WriteLn(Format('Failed in sem_destroy, (err: %d)', [fpgeterrno]));
  {$ENDIF}
  Dispose(Sem);

  inherited Destroy;
end;

procedure TSemaphore.Inc;
begin
  if sem_post(Sem) <> 0 then
  begin
    ALoggerUnit.GetLogger.FmtFatalLn('Failed in getting value, (err: %d)', [fpgeterrno]);
  end;

end;

procedure TSemaphore.Dec;
begin
  if sem_wait(Sem) <> 0 then
  begin
    ALoggerUnit.GetLogger.FatalLn(
      Format('Failed in getting value, (err: %d)', [fpgeterrno]));
  end;
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
  {$IFNDEF SYNC_MACOS}
  // On macOS, DoneCriticalSection may fail due to semaphore cleanup issues
  // Skip cleanup - OS will handle it on process exit
  DoneCriticalSection(CS);
  {$ENDIF}

  inherited Destroy;
end;

end.
