unit QueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HeapUnit, cthreads, GenericCollectionUnit, SyncUnit;

type

  { EQueueIsFull }

  EQueueIsFull = class(Exception)
  public
    constructor Create;

  end;

  { EQueueIsEmpty }

  EQueueIsEmpty = class(Exception)
  public
    constructor Create;

  end;

  { TGenericAbstractQueue }

  {
    This is the base class for all Generic Queues.

    Jan 1, 2013: I had to make all the abstract functions, explicit to functions
      to avoid some weird compile errors.
  }
  generic TGenericAbstractQueue<T> = class(TObject)
  private
    { Returns the number of elemnts in the Queue }
    function GetCount: Integer; virtual;
    { Checks if Queue is empty }
    function GetIsEmpty: Boolean; virtual;
    { Checks if Queue is full }
    function GetIsFull: Boolean; virtual;

  protected
    { The actual code for Insert goes here }
    procedure DoInsert(Entry: T); virtual; abstract;
    { The actual code for Delete goes here }
    procedure DoDelete(var LastElement: T); virtual; abstract;
    { The actual code for GetTop goes here}
    function DoGetTop: T; virtual;
  public
    property Count: Integer read GetCount;
    property IsFull: Boolean read GetIsFull;
    property IsEmpty: Boolean read GetIsEmpty;

    procedure Insert(Entry: T); virtual;
    procedure Delete(var LastElement: T); virtual;
    function GetTop: T; virtual;
    { Removes all the objects from Queue (Does not free the objects) }
    procedure Clear; virtual;

    constructor Create;
    destructor Destroy; override;

  end;

  { TGenericCircularQueue }

  generic TGenericCircularQueue<T> = class(specialize TGenericAbstractQueue<T>)
  private
    FData: array of T;
    SoQ, EoQ: Integer;
    FCount: Integer;

  private
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;

    procedure Clear; override;


  end;

  { TGenericQueue }

  generic TGenericQueue<T> = class(specialize TGenericAbstractQueue<T>)
  private
  type
    TData = specialize TCollection<T>;
  private
    FData: TData;

  private
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  end;

  { TThreadSafeGenericQueue }

  generic TThreadSafeGenericQueue<T> = class(specialize TGenericQueue<T>)
  private
    Mutex: TMutex;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  end;

  { TGenericBlockingQueue }

  generic TGenericBlockingQueue<T> = class(specialize TGenericQueue<T>)
  private type
    TState = (stNotStart, stAccepting, stFreeing);
  private
    State: TState;
    Mutex: TMutex;
    FullSlots: TSemaphore;

    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;

  public
    constructor Create;
    destructor Destroy; override;

  end;

    (*
  { TGenericPriorityQueue }

  generic TGenericPriorityQueue<T> = class(specialize TGenericAbstractQueue<T>)
  private
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;

  type
    TMinHeapT = specialize THeap<T>;

  protected
    MinHeap: TMinHeapT;

  public

    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  end;
  *)

implementation

uses
  ALoggerUnit;

  { TThreadSafeGenericQueue }

function TThreadSafeGenericQueue.GetCount: Integer;
begin
  Result := inherited GetCount;
end;

function TThreadSafeGenericQueue.GetIsEmpty: Boolean;
begin
  Mutex.Lock;

  Result := inherited GetIsEmpty;

  Mutex.Unlock;
end;

function TThreadSafeGenericQueue.GetIsFull: Boolean;
begin
  Mutex.Lock;

  Result := inherited GetIsFull;

  Mutex.Unlock;
end;

procedure TThreadSafeGenericQueue.DoInsert(Entry: T);
begin
  Mutex.Lock;

  inherited DoInsert(Entry);

  Mutex.Unlock;
end;

procedure TThreadSafeGenericQueue.DoDelete(var LastElement: T);
begin
  Mutex.Lock;

  inherited DoDelete(LastElement);

  Mutex.Unlock;

end;

function TThreadSafeGenericQueue.DoGetTop: T;
begin
  Mutex.Lock;

  Result := inherited DoGetTop;

  Mutex.Unlock;

end;

constructor TThreadSafeGenericQueue.Create;
begin
  inherited Create;

  Mutex := TMutex.Create;

end;

destructor TThreadSafeGenericQueue.Destroy;
begin
  Mutex.Lock;
  Mutex.Unlock;

  Mutex.Free;

  inherited Destroy;
end;

procedure TThreadSafeGenericQueue.Clear;
begin
  Mutex.Lock;

  inherited Clear;

  Mutex.Unlock;
end;

{ TGenericBlockingQueue }

function TGenericBlockingQueue.GetCount: Integer;
begin
  Mutex.Lock;

  Result :=inherited GetCount;

  Mutex.Unlock;
end;

function TGenericBlockingQueue.GetIsEmpty: Boolean;
begin
  Mutex.Lock;

  Result :=inherited GetIsEmpty;

  Mutex.Unlock;
end;

function TGenericBlockingQueue.GetIsFull: Boolean;
begin
  Mutex.Lock;

  Result :=inherited GetIsFull;

  Mutex.Unlock;
end;

procedure TGenericBlockingQueue.DoInsert(Entry: T);
begin
  Mutex.Lock;
  inherited DoInsert(Entry);
  Mutex.Unlock;

  FullSlots.Inc(1);
end;

procedure TGenericBlockingQueue.DoDelete(var LastElement: T);
begin
  FullSlots.Dec(1);

  Mutex.Lock;

  inherited DoDelete(LastElement);

  Mutex.Unlock;

end;

function TGenericBlockingQueue.DoGetTop: T;
begin
  Mutex.Lock;

  Result := inherited DoGetTop;

  Mutex.Unlock;
end;

constructor TGenericBlockingQueue.Create;
begin
  inherited Create;

  State := stNotStart;
  Mutex := TMutex.Create;
  FullSlots := TSemaphore.Create(0);

  State := stAccepting;
end;

destructor TGenericBlockingQueue.Destroy;
begin

  State := stFreeing;
  FullSlots.Free;
  Mutex.Free;

  inherited Destroy;
end;

{ TGenericPriorityQueue }
(*
function TGenericPriorityQueue.DoGetTop: T;
begin
  Result := MinHeap.Min;

end;

function TGenericPriorityQueue.GetCount: Integer;
begin
  Result := MinHeap.Count;

end;

function TGenericPriorityQueue.GetIsEmpty: Boolean;
begin
  Result := MinHeap.Count = 0;

end;

function TGenericPriorityQueue.GetIsFull: Boolean;
begin
  Result := False;

end;

procedure TGenericPriorityQueue.DoInsert(Entry: T);
begin
  MinHeap.Insert(Entry);

end;

procedure TGenericPriorityQueue.DoDelete(var LastElement: T);
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  LastElement := MinHeap.Min;
  MinHeap.DeleteMin;

end;

constructor TGenericPriorityQueue.Create;
begin
  inherited Create;

  MinHeap := TMinHeapT.Create;

end;

destructor TGenericPriorityQueue.Destroy;
begin
  MaxHeap.Free;

end;

procedure TGenericPriorityQueue.Clear;
begin
  MaxHeap.Clear;

end;
*)

{ TGenericAbstractQueue }

function TGenericAbstractQueue.GetCount: Integer;
begin
  Result := 0;

end;

function TGenericAbstractQueue.GetIsEmpty: Boolean;
begin
  Result := False;

end;

function TGenericAbstractQueue.GetIsFull: Boolean;
begin
  Result := False;

end;

function TGenericAbstractQueue.DoGetTop: T;
begin
  Result := Default(T);

end;

procedure TGenericAbstractQueue.Insert(Entry: T);
begin
  if IsFull then
    raise EQueueIsFull.Create;

  DoInsert(Entry);

end;

procedure TGenericAbstractQueue.Delete(var LastElement: T);
begin
  DoDelete(LastElement);

end;

function TGenericAbstractQueue.GetTop: T;
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  Result := DoGetTop;

end;

procedure TGenericAbstractQueue.Clear;
begin

end;

constructor TGenericAbstractQueue.Create;
begin
  inherited Create;

end;

destructor TGenericAbstractQueue.Destroy;
begin
  inherited Destroy;
end;


{ EQueueIsFull }

constructor EQueueIsFull.Create;
begin
  inherited Create('Queue is full!');

end;

{ EQueueIsEmpty }

constructor EQueueIsEmpty.Create;
begin
  inherited Create('Queue is empty!');

end;

{ TGenericCircularQueue }

function TGenericCircularQueue.GetCount: Integer;
begin
  Result := FCount;

end;

function TGenericCircularQueue.GetIsEmpty: Boolean;
begin
  Result := SoQ = EoQ;

end;

function TGenericCircularQueue.GetIsFull: Boolean;
begin
  Result := (SoQ = ((EoQ + 1) mod Count));

end;

function TGenericCircularQueue.DoGetTop: T;
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  Result := FData[SoQ];

end;

procedure TGenericCircularQueue.DoInsert(Entry: T);
begin
  if IsFull then
    raise EQueueIsFull.Create;


  FData[EoQ] := Entry;
  EoQ := (EoQ + 1) mod Count;

end;

procedure TGenericCircularQueue.DoDelete(var LastElement: T);
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  LastElement := FData[SoQ];
  SoQ := (SoQ + 1) mod Count;

end;

constructor TGenericCircularQueue.Create(Size: Integer);
begin
  inherited Create();

  SetLength(FData, Size);
  FCount := Size;
  SoQ := 0;
  EoQ := 0;

end;

destructor TGenericCircularQueue.Destroy;
begin
  SetLength(FData, 0);

  inherited;

end;

procedure TGenericCircularQueue.Clear;
begin
  SetLength(FData, 0);
  FCount := 0;

end;

{ TGenericQueue }

procedure TGenericQueue.DoInsert(Entry: T);
begin
  if IsFull then
    raise EQueueIsFull.Create;

  FData.Add(Entry);

end;

procedure TGenericQueue.DoDelete(var LastElement: T);
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  LastElement := FData[0];
  FData.Delete(0);

end;

constructor TGenericQueue.Create;
begin
  inherited Create;

  FData := TData.Create;
end;

destructor TGenericQueue.Destroy;
begin
  FData.Free;

  inherited Destroy;

end;

procedure TGenericQueue.Clear;
begin
  FData.Clear;

end;

function TGenericQueue.GetCount: Integer;
begin
  Result := FData.Count;

end;

function TGenericQueue.GetIsEmpty: Boolean;
begin
  Result := Count = 0;

end;

function TGenericQueue.GetIsFull: Boolean;
begin
  Result := False;

end;

function TGenericQueue.DoGetTop: T;
begin
  Result := T(FData[0]);

end;

end.
