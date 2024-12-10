unit ThreadSafeStackUnit;

{$mode ObjFPC}{$H+}

interface
uses
  GenericCollectionUnit, SyncUnit;

type
  { TThreadSafeStack }
  {
   The implementation of Delete method, in this class, is blocking, meaning
  the caller will be blocked if there is no element in the Queue.
  }

  { TBaseStack }

  generic TBaseStack<T> = class(TObject)
  private
    function GetCount: Integer; virtual; abstract;
    function GetIsEmpty: Boolean; virtual; abstract;
    function GetIsFull: Boolean; virtual; abstract;
  protected
    procedure DoPush(Entry: T); virtual; abstract;
    procedure DoPop(var LastElement: T); virtual; abstract;

  public
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;

    procedure Push(Entry: T);
    function Pop: T;
  end;

  generic TThreadSafeStack<T> = class(specialize TBaseStack<T>)
  private type
    TDataList = specialize TCollection<T>;
  private
    FData: TDataList;
    Mutex: TMutex;
    Semaphore: TSemaphore;
    FDone: Boolean;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;
    procedure SetDone(AValue: Boolean);

  protected
    procedure DoPush(Entry: T); override;
    procedure DoPop(var LastElement: T); override;

  public
    property Done: Boolean read FDone write SetDone;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadSafeStack }

function TThreadSafeStack.GetCount: Integer;
begin
  Result := Semaphore.Value;

end;

function TThreadSafeStack.GetIsEmpty: Boolean;
begin
  Result := Count = 0;

end;

function TThreadSafeStack.GetIsFull: Boolean;
begin
  Result := False;

end;

procedure TThreadSafeStack.SetDone(AValue: Boolean);
begin
  if FDone=AValue then Exit;
  if not AValue then
    Exit;

  FDone := AValue;
  // TODO: We need to keep track of the difference between number of Pop
  // and number of Pushes.
  Semaphore.Inc(1024);

end;

procedure TThreadSafeStack.DoPush(Entry: T);
begin
  WriteLn('DoPush: ', ThreadID);
  Mutex.Lock;
  WriteLn('DoPush: ', ThreadID);

  FData.Add(Entry);

  Semaphore.Inc(1);
  Mutex.Unlock;
  WriteLn('DoPush: ', ThreadID);

end;

procedure TThreadSafeStack.DoPop(var LastElement: T);
begin
  FillChar(LastElement, SizeOf(T), 0);

  while True do
  begin
    WriteLn('DoPop: ', ThreadID);
    Semaphore.Dec(1);
    WriteLn('DoPop: ', ThreadID);

    Mutex.Lock;
    WriteLn('DoPop: ', ThreadID);
    if (FData.Count = 0) and Done then
    begin
      Mutex.Unlock;
      Exit;

    end;

    if 0 < FData.Count then
    begin
      LastElement := FData.Last;
      FData.Count := FData.Count - 1;

      Mutex.Unlock;

      Exit;
    end;

    Mutex.Unlock;
  end;

end;

constructor TThreadSafeStack.Create;
begin
  inherited;

  FData := TDataList.Create;
  Mutex := TMutex.Create;
  Semaphore := TSemaphore.Create(0);
  FDone := False;

end;

destructor TThreadSafeStack.Destroy;
begin
  Done := True;

  FData.Free;
  Mutex.Free;
  Semaphore.Free;

  inherited Destroy;
end;

{ TBaseStack }

procedure TBaseStack.Push(Entry: T);
begin
  DoPush(Entry);

end;

function TBaseStack.Pop: T;
begin
  DoPop(Result);

end;

end.

