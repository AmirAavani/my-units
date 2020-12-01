unit ThreadSafeQueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, QueueUnit, SyncUnit, fgl;

type
  { TThreadSafeQueue }
  {
   The implementation of Delete method, in this class, is blocking, meaning
  the caller will be blocked if there is no element in the Queue.
  }

  generic TThreadSafeQueue<T> = class(specialize TGenericAbstractQueue<T>)
  private type
    TDataList = specialize TFPGList<T>;
  private
    FData: TDataList;
    Mutex: TMutex;
    Semaphore: TSemaphore;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;

  public
    procedure Clear; override;

    constructor Create;
    destructor Destroy; override;
  end;


implementation
uses
  ALoggerUnit;

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
  DebugLn(Format('Count: %d', [Semaphore.Value]));
  DebugLn(Format('DoInsert M.Lock!', []));
  Mutex.Lock;

  DebugLn(Format('M.Lock passed!', []));
  DebugLn(Format('Count: %d', [Semaphore.Value]));

  FData.Add(Entry);

  DebugLn(Format('Sending Signal!', []));

  Semaphore.Inc;
  Mutex.Unlock;

  DebugLn(Format('Count: %d', [Semaphore.Value]));

end;

procedure TThreadSafeQueue.DoDelete(var LastElement: T);
begin
   LastElement := nil;
   DebugLn(Format('Who am I?', []));
   while LastElement = nil do
   begin
     DebugLn(Format('Count: %d', [Semaphore.Value]));
     DebugLn(Format('Before Semaphore.Dec', []));

     Semaphore.Dec;
     DebugLn(Format('Count: %d', [Semaphore.Value]));

     DebugLn(Format('Do.Delete M.Lock!', []));
     Mutex.Lock;
     DebugLn(Format('M.Lock passed!', []));
     DebugLn(Format('FData.Count = %d ', [FData.Count]));

     if 0 < FData.Count then
     begin
       LastElement := FData.First;
       FData.Delete(0);

       DebugLn(Format('FData.Count = %d ', [FData.Count]));
       Mutex.Unlock;

       Break;
     end;

     DebugLn(Format('M.UnLock', []));
     Mutex.Unlock;
     DebugLn(Format('Count: %d', [Semaphore.Value]));
   end;

end;

function TThreadSafeQueue.DoGetTop: TObject;
begin
   Result := nil;
  raise Exception.Create('Not Implemented Yet');

end;

procedure TThreadSafeQueue.Clear;
begin
  raise Exception.Create('Not Implemented yet!');

end;

constructor TThreadSafeQueue.Create;
begin
  inherited Create;

  FData := TDataList.Create;
  Mutex := TMutex.Create;
  Semaphore := TSemaphore.Create(0);
end;

destructor TThreadSafeQueue.Destroy;
begin
  Mutex.Free;
  FData.Free;
  Semaphore.Free;

  inherited Destroy;
end;

end.

