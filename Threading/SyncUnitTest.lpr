program SyncUnitTest;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, sysutils, SyncUnit
  { you can add units after this };

type

  { TSemaphoreTest }

  TSemaphoreTest = class(TThread)
  private
    Semaphore: TSemaphore;
    ShouldIncrement: Boolean;
    ID: Integer;
  public
    constructor Create(_ID: Integer; Sem: TSemaphore; Increment: Boolean);
    destructor Destroy; override;

    procedure Execute; override;

  end;

  { TReader }

  TReader = class(TThread)
  public
    constructor Create;
    procedure Execute; override;
  end;

  { TWriter }

  TWriter = class(TThread)
  public
    procedure Execute; override;

  end;

function GetMS(const DT: TDateTime): Comp;
begin
  Result := TimeStampToMSecs(DateTimeToTimeStamp(DT));
end;

{ TSemaphoreTest }

constructor TSemaphoreTest.Create(_ID: Integer; Sem: TSemaphore;
  Increment: Boolean);
begin
  inherited Create(True);

  ID := _ID;
  Semaphore := Sem;
  ShouldIncrement := Increment;
end;

destructor TSemaphoreTest.Destroy;
begin
  inherited Destroy;
end;

procedure TSemaphoreTest.Execute;
begin
  WriteLn(Format('I am %d: (CurVal: %d)', [ID, Semaphore.Value]));
  if ShouldIncrement then
  begin
    while True do
    begin
      Sleep(1000);
      WriteLn(Format('%d: I am %d: Going to Increase (CurVal: %d)', [DateTimeToTimeStamp(Now).Time, ID, Semaphore.Value]));
      Semaphore.Inc;
      WriteLn(Format('%d: I am %d: Just Incremented (CurVal: %d)', [DateTimeToTimeStamp(Now).Time, ID, Semaphore.Value]));
     end;
     Exit;

  end;

  while True do
  begin
    Sleep(50);
    WriteLn(Format('%d: I am %d: Going to Decrease (CurVal: %d)', [DateTimeToTimeStamp(Now).Time, ID, Semaphore.Value]));
    Semaphore.Dec;
    WriteLn(Format('%d: I am %d: Just Decremented (CurVal: %d)', [DateTimeToTimeStamp(Now).Time, ID, Semaphore.Value]));

  end;

end;

{ TWriter }

procedure TWriter.Execute;
begin

end;

{ TReader }

constructor TReader.Create;
begin

end;

procedure TReader.Execute;
begin

end;


procedure RunSemaphoreTest;
var
  Sem: TSemaphore;
  Start: Comp;
  Threads: array of TThread;
  i: Integer;

begin
  Sem := TSemaphore.Create(0);
  Start := TimeStampToMSecs(DateTimeToTimeStamp(Now));

  SetLength(Threads, 10);
  for i := 0 to High(Threads) do
  begin
    Threads[i] := TSemaphoreTest.Create(i + 1, Sem, Odd(i));
    Threads[i].Resume;
  end;

  while TimeStampToMSecs(DateTimeToTimeStamp(Now)) - Start < 60 * 1000 do
  begin
    Sleep(1000);
  end;

end;

begin
  RunSemaphoreTest;
end.

