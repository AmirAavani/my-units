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
  Mutex.Lock;

  FData.Add(Entry);

  Semaphore.Inc;
  Mutex.Unlock;

end;

procedure TThreadSafeQueue.DoDelete(var LastElement: T);
begin
   LastElement := nil;

   while LastElement = nil do
   begin
     Semaphore.Dec;

     Mutex.Lock;

     if 0 < FData.Count then
     begin
       LastElement := FData.First;
       FData.Delete(0);

       Mutex.Unlock;

       Break;
     end;

     Mutex.Unlock;
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

