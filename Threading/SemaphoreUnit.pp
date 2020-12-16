unit SemaphoreUnit;

{$mode objfpc}{$H+}

interface

uses
  Contnrs, Classes, SysUtils;

type

  { TSemaphore }

  TSemaphore = class
  private
    FMaxPermits: Cardinal;
    FPermits: Cardinal;
    FLock: TRTLCriticalSection;
    FBlockQueue: Contnrs.TQueue;

    function GetWaitCount: Cardinal;

  public
    property WaitCount: Cardinal read GetWaitCount;
    property Permits: Cardinal read FPermits;
    property MaxPermits: Cardinal read FMaxPermits;

    procedure Wait;
    procedure Post;
    function Used: Boolean;

    constructor Create(_MaxPermits: Cardinal);
    destructor Destroy; override;
  end;


implementation

{ TSemaphore }

function TSemaphore.GetWaitCount: Cardinal;
begin
  EnterCriticalSection(FLock);

  try
    Result:= FBlockQueue.Count;

  finally
    LeaveCriticalSection(FLock);

  end;

end;

procedure TSemaphore.Wait;
var
  aWait: Boolean;
  aEvent: RTLEvent;

begin
  EnterCriticalSection(FLock);

  try
    if 0 < FPermits then
    begin
      Dec(FPermits);
      aWait := False;

    end
    else
    begin
      New(aEvent);
      aEvent := RTLEventCreate;
      FBlockQueue.Push(aEvent);
      aWait := True;

    end;

  finally
    LeaveCriticalSection(FLock);

  end;

  if aWait then
  begin
    DebugLn('Sempahore Await');
    RTLeventWaitFor(aEvent);
    RTLEventDestroy(aEvent);

  end;

end;

procedure TSemaphore.Post;
begin
  EnterCriticalSection(FLock);
  try
    if 0 < FBlockQueue.Count then
      RTLEventSetEvent(PRTLEvent(FBlockQueue.Pop))
    else
      Inc(FPermits);

  finally
    LeaveCriticalSection(fLock);

  end;
end;

function TSemaphore.Used: Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result := FPermits < FMaxPermits;

  finally
    LeaveCriticalSection(FLock);

  end;
end;

constructor TSemaphore.Create(_MaxPermits: Cardinal);
begin
  FMaxPermits := _MaxPermits;
  FPermits := _MaxPermits;
  InitCriticalSection(FLock);
  FBlockQueue := TQueue.Create;

end;

destructor TSemaphore.Destroy;
begin
  DoneCriticalSection(FLock);
  FBlockQueue.Free;

  inherited Destroy;
end;

end.

