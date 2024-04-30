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

  private
    FData: TDataList;
    FEndOfOperation: Boolean;
    Mutex: TMutex;
    Semaphore: TSemaphore;
    wg: TWaitGroup;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;

  public
    property EndOfOperation: Boolean read FEndOfOperation;

    procedure Clear; override;

    constructor Create;
    destructor Destroy; override;
  end;

  { EInsertion }

  EInsertion = class (Exception)
  public
    constructor RealCreate(Msg: AnsiString);
    constructor Create;
    constructor Create(Msg: AnsiString);

  end;


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
  FmtFatalLnIFFalse(
    False,
    'You cannot create this object out of this unit',
    []);

end;

constructor EInsertion.Create(Msg: AnsiString);
begin
  inherited Create(Msg);

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

procedure TThreadSafeQueue.DoInsert(Entry: T);
begin
  Mutex.Lock;

  if Self.FEndOfOperation then
    raise EInsertion.RealCreate('Cannot Insert New Entry After Setting EndOfOpretion');

  FData.Add(Entry);

  Semaphore.Inc;
  Mutex.Unlock;

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
    if FEndOfOperation then
    begin
      Break;

    end;

    Mutex.Lock;

    if 0 < FData.Count then
    begin
      LastElement := FData.First;
      HasElement := True;
      FData.Delete(0);
      Mutex.Unlock;
      Break;
    end
    else if Self.FEndOfOperation then
    begin
     Mutex.Unlock;
     Exit;

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
  Semaphore.Inc(wg.GetValue);

  wg.Wait;
  wg.Free;

  Mutex.Free;
  FData.Free;
  Semaphore.Free;

  inherited Destroy;
end;

end.

