unit SyncUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cthreads, pthreads;

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

    FValue: Integer;
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

implementation

uses
  BaseUnix;

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
  if sem_wait(Sem) <> 0 then
    WriteLn(Format('Failed in getting value, (err: %d)', [fpgeterrno]));
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

