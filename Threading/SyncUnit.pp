unit SyncUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cthreads, pthreads, Contnrs;

type

  { TMutex }

  TMutex = class(TObject)
  private
    CS: TRTLCriticalSection;
  public
    procedure Lock;
    procedure Unlock;

    constructor Create;
    destructor Destroy; override;
  end;

  { TSemaphore }

  TSemaphore = class(TObject)
  private
    Sem: psem_t;

    function GetValue: Integer;

  public
    property Value: Integer read GetValue;

    constructor Create(const v: Integer = 1);
    destructor Destroy; override;

    procedure Inc;
    procedure Dec;

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

    procedure Wait;
  end;

implementation

uses
  ALoggerUnit, BaseUnix;

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
  DebugLn('Done');
  Mutex.Lock;
  DebugLn(Format('Value : %d', [Value]));

  Value -= k;
  DebugLn(Format('Value : %d', [Value]));

  if Value < 0 then
     FatalLn(Format('Value(%d) < 0', [Value]));

  if Value = 0 then
    for i := 1 to BlockQueue.Count do
      RTLEventSetEvent(PRTLEvent(BlockQueue.Pop));

  Mutex.Unlock;

end;

procedure TWaitGroup.Wait;
var
  aWait: Boolean;
  anEvent: PRTLEvent;

begin
  DebugLn('Wait');
  Mutex.Lock;
  DebugLn(Format('Value : %d', [Value]));

  aWait:= False;
  if 0 < Value then
  begin
    DebugLn(Format('Waiting Value : %d', [Value]));
    anEvent := RTLEventCreate;
    BlockQueue.Push(anEvent);
    aWait := True;
    DebugLn(Format('Waiting Value : %d', [Value]));

  end;

  Mutex.Unlock;
  DebugLn('Unlocked');

  if aWait then
  begin
    DebugLn('aWait');
    RTLeventWaitFor(anEvent);
    DebugLn('aWait');
    RTLEventDestroy(anEvent);
    DebugLn('aWait');

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

  DebugLn('Before New');
  New(Sem);
  if sem_init(Sem, 0, v) <> 0 then
    WriteLn(Format('Failed in sem_init, (err: %d)', [fpgeterrno]));
end;

destructor TSemaphore.Destroy;
begin
  if sem_destroy(Sem) <> 0 then
    WriteLn(Format('Failed in sem_destroy, (err: %d)', [fpgeterrno]));
  Dispose(Sem);

  inherited Destroy;
end;

procedure TSemaphore.Inc;
begin
  if sem_post(Sem) <> 0 then
    WriteLn(Format('Failed in getting value, (err: %d)', [fpgeterrno]));
end;

procedure TSemaphore.Dec;
begin
  DebugLn('Before Dec');
  if sem_wait(Sem) <> 0 then
  begin
    DebugLn('-Before Dec');
    FatalLn(Format('Failed in getting value, (err: %d)', [fpgeterrno]));
  end;
  DebugLn('+Before Dec');
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

