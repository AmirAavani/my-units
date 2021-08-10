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
    function DoGetTop: T; virtual; abstract;

  public
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;

    procedure Push(Entry: T);
    function Pop: T;
    function GetTop: T;
  end;

  generic TThreadSafeStack<T> = class(specialize TBaseStack<T>)
  private type
    TDataList = specialize TCollection<T>;
  private
    FData: TDataList;
    Mutex: TMutex;
    Semaphore: TSemaphore;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoPush(Entry: T); override;
    procedure DoPop(var LastElement: T); override;
    function DoGetTop: T; override;

  public
    procedure Push(Entry: T);

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

procedure TThreadSafeStack.DoPush(Entry: T);
begin
  Mutex.Lock;

  FData.Add(Entry);

  Semaphore.Inc;
  Mutex.Unlock;

end;

procedure TThreadSafeStack.DoPop(var LastElement: T);
begin
  LastElement := nil;

  while LastElement = nil do
  begin
    Semaphore.Dec;

    Mutex.Lock;

    if 0 < FData.Count then
    begin
      LastElement := FData.Last;
      FData.Count := FData.Count - 1;

      Mutex.Unlock;

      Break;
    end;

    Mutex.Unlock;
  end;

end;

function TThreadSafeStack.DoGetTop: T;
begin

end;

procedure TThreadSafeStack.Push(Entry: T);
begin

end;

constructor TThreadSafeStack.Create;
begin

end;

destructor TThreadSafeStack.Destroy;
begin
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

function TBaseStack.GetTop: T;
begin
  Result := DoGetTop;

end;

end.

