unit ThreadSafeQueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, QueueUnit, SyncUnit, GenericCollectionUnit;

type
  { TThreadSafeQueue }
  {
   The implementation of Delete method, in this class, is blocking, meaning
  the caller will be blocked if there is no element in the Queue.
  }

  generic TThreadSafeQueue<T> = class(specialize TGenericAbstractQueue<T>)
  private type
    TDataList = specialize TCollection<T>;
    TAtomicInt = specialize TAtomic<Integer>;
  private
    FData: TDataList;
    FEndOfOperation: Boolean;
    Mutex: TMutex;
    Semaphore: TSemaphore;
    wg: TWaitGroup;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;
    procedure SetEndOfOperation(AValue: Boolean);

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;

  public
    procedure Clear; override;
    property EndOfOperation: Boolean read FEndOfOperation write SetEndOfOperation;

    constructor Create;
    destructor Destroy; override;
  end;

  { EInsertion }

  EInsertion = class (Exception)
  private
    constructor RealCreate(Msg: AnsiString);
  public
    constructor Create;
    constructor Create(Msg: AnsiString);

  end;


function IncAtomicInt(v: Integer): Integer;
function DecAtomicInt(v: Integer): Integer;

implementation
uses
  ALoggerUnit;

{ EInsertion }

constructor EInsertion.RealCreate(Msg: AnsiString);
begin
  inherited Create(Msg);
end;

constructor EInsertion.Create;
begin
  FatalLn('You cannot create this object out of this unit');

end;

constructor EInsertion.Create(Msg: AnsiString);
begin
  Create;

end;

{ TThreadSafeQueue }

function TThreadSafeQueue.GetCount: Integer;
begin
  Result := Semaphore.Value;

end;

function TThreadSafeQueue.GetIsEmpty: Boolean;
begin
  Result := Count = 0;

end;

function TThreadSafeQueue.GetIsFull: Boolean;
begin
  Result := False;

end;

procedure TThreadSafeQueue.SetEndOfOperation(AValue: Boolean);
begin
  if FEndOfOperation = AValue then Exit;

  if FEndOfOperation and not AValue then
  begin
    FmtFatalLn('Cannot Undo EndOfOperation!', []);

  end;

  Mutex.Lock;

  FEndOfOperation := AValue;

  Mutex.Unlock;
end;

procedure TThreadSafeQueue.DoInsert(Entry: T);
begin
  Mutex.Lock;

  if Self.EndOfOperation then
    raise EInsertion.RealCreate('Cannot Insert New Entry After Setting EndOfOpretion');

  FData.Add(Entry);

  Semaphore.Inc;
  Mutex.Unlock;

end;

function IncAtomicInt(v: Integer): Integer;
begin
  Result := v + 1;

end;

function DecAtomicInt(v: Integer): Integer;
begin
  Result := v - 1;

end;

procedure TThreadSafeQueue.DoDelete(var LastElement: T);
var
  HasElement: Boolean;

begin
   wg.Add(1);

   LastElement := Default(T);
   HasElement := False;

   while not HasElement do
   begin
     Semaphore.Dec;
     if EndOfOperation then
       Break;

     Mutex.Lock;

     if Self.EndOfOperation then
     begin
       Mutex.Unlock;
       Break;

     end;

     if 0 < FData.Count then
     begin
       LastElement := FData.First;
       HasElement := True;
       FData.Delete(0);

       Mutex.Unlock;

       Break;
     end;

     Mutex.Unlock;
   end;

   wg.Done(1);

end;

function TThreadSafeQueue.DoGetTop: T;
begin
  Result := Default(T);
  raise Exception.Create('Not Implemented Yet');

end;

procedure TThreadSafeQueue.Clear;
begin
  raise Exception.Create('Not Implemented yet!');

end;

constructor TThreadSafeQueue.Create;
begin
  inherited Create;

  FEndOfOperation := False;
  FData := TDataList.Create;
  Mutex := TMutex.Create;
  Semaphore := TSemaphore.Create(0);
  wg := TWaitGroup.Create;

end;

destructor TThreadSafeQueue.Destroy;
begin
  FEndOfOperation := True;
  FMTDebugLn('SuspendedThreads: %d', [wg.GetValue]);
  Semaphore.Inc(wg.GetValue);

  wg.Wait;
  FMTDebugLn('SuspendedThreads: %d', [wg.GetValue]);
  wg.Free;
  Mutex.Free;
  FData.Free;
  Semaphore.Free;

  inherited Destroy;
end;

end.

