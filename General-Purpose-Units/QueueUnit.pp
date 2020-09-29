unit QueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HeapUnit, cthreads, fgl, SyncUnit;
  
type

  { EQueueIsFull }

  EQueueIsFull= class(Exception)
  public
    constructor Create;
    
  end;
  
  { EQueueIsEmpty }

  EQueueIsEmpty= class(Exception)
  public
    constructor Create;

  end;

  { TGenericAbstractQueue }

  {
    This is the base class for all Generic Queues.

    Jan 1, 2013: I had to make all the abstract functions, explicit to functions
      to avoid some weird compile errors.
  }
  generic TGenericAbstractQueue<T>= class(TObject)
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

  generic TGenericCircularQueue<T>= class(specialize TGenericAbstractQueue <T>)
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

    procedure Clear;


  end;

  { TGenericQueue }

  generic TGenericQueue<T>= class(specialize TGenericAbstractQueue<T>)
  private
    FData: TList;

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
    destructor Destroy;
    procedure Clear;

  end;

  { TGenericPriorityQueue }

  generic TGenericPriorityQueue<T>= class(specialize TGenericAbstractQueue<T>)
  private
    function GetCount: Integer; override;
    function GetIsEmpty: Boolean; override;
    function GetIsFull: Boolean; override;

  protected
    procedure DoInsert(Entry: T); override;
    procedure DoDelete(var LastElement: T); override;
    function DoGetTop: T; override;

  type
    TIsGreaterThanFunction= function(const a: T; const b: T): Boolean;
    TMaxHeapT= specialize THeap<T>;

  protected
    IsGreaterThan: TIsGreaterThanFunction;
    MaxHeap: TMaxHeapT;

  public

    constructor Create(IsGreaterThanFunction: TIsGreaterThanFunction);
    destructor Destroy; override;
    procedure Clear; override;

  end;

  { TThreadSafeQueue }
  {
   The implementation of Delete method, in this class, is blocking, meaning
  the caller will be blocked if there is no element in the Queue.
  }

  generic TThreadSafeQueue<T> = class(specialize TGenericAbstractQueue<T>)
  private type
    TDataList = specialize TFPGList<T>;
  private
    FCount: Integer;
    FData: TDataList;
    NonEmptyBlockEvent: PRTLEvent;
    Mutex: TMutex;

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
  Mutex.Lock;
  Result := FCount;
  Mutex.Unlock;

end;

function TThreadSafeQueue.GetIsEmpty: Boolean;
begin
  Result := FCount = 0;

end;

function TThreadSafeQueue.GetIsFull: Boolean;
begin
  Result := False;

end;

procedure TThreadSafeQueue.DoInsert(Entry: T);
begin
  WriteLn('Who am I?');
  WriteLn('M.Lock!');
  Mutex.Lock;
  WriteLn('M.Lock passed!');

  FData.Add(Entry);

  RTLeventSetEvent(NonEmptyBlockEvent);
  Mutex.Unlock;

end;

procedure TThreadSafeQueue.DoDelete(var LastElement: T);
begin
   LastElement := nil;
   WriteLn('Who am I?');
   while LastElement = nil do
   begin
     WriteLn('Waiting for Signal!');
     RTLeventWaitFor(NonEmptyBlockEvent);
     WriteLn('Signal Recieved!');

     WriteLn('M.Lock!');
     Mutex.Lock;
     WriteLn('M.Lock passed!');
     WriteLn('FData.Count = ' + IntToStr(FData.Count));

     if 0 < FData.Count then
     begin
       LastElement := FData.Last;
       FData.Delete(FData.Count - 1);
       Mutex.Unlock;

       Break;
     end;

     WriteLn('M.UnLock');
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
  NonEmptyBlockEvent := RTLEventCreate;

end;

destructor TThreadSafeQueue.Destroy;
begin
  Mutex.Free;

  FData.Free;
  RTLeventdestroy(NonEmptyBlockEvent);

  inherited Destroy;
end;

{ TGenericPriorityQueue }

function TGenericPriorityQueue.DoGetTop: T;
begin
  Result:= MaxHeap.Min;

end;

function TGenericPriorityQueue.GetCount: Integer;
begin
  Result:= MaxHeap.Count;

end;

function TGenericPriorityQueue.GetIsEmpty: Boolean;
begin
  Result:= MaxHeap.Count= 0;

end;

function TGenericPriorityQueue.GetIsFull: Boolean;
begin
  Result:= False;

end;

procedure TGenericPriorityQueue.DoInsert(Entry: T);
begin
  MaxHeap.Insert(Entry);

end;

procedure TGenericPriorityQueue.DoDelete(var LastElement: T);
begin
   if IsEmpty then
     raise EQueueIsEmpty.Create;

  LastElement := MaxHeap.Min;
  MaxHeap.DeleteMin;

end;

constructor TGenericPriorityQueue.Create(
      IsGreaterThanFunction: TIsGreaterThanFunction);
begin
  inherited Create;

  IsGreaterThan:= IsGreaterThanFunction;
  MaxHeap:= TMaxHeapT.Create(IsGreaterThanFunction);

end;

destructor TGenericPriorityQueue.Destroy;
begin
  MaxHeap.Free;

end;

procedure TGenericPriorityQueue.Clear;
begin
  MaxHeap.Clear;

end;

{ TGenericAbstractQueue }

function TGenericAbstractQueue.GetCount: Integer;
begin
  Result:= 0;

end;

function TGenericAbstractQueue.GetIsEmpty: Boolean;
begin
  Result:= False;

end;

function TGenericAbstractQueue.GetIsFull: Boolean;
begin
  Result:= False;

end;

function TGenericAbstractQueue.DoGetTop: T;
begin
  Result:= nil;

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

  Result:= DoGetTop;

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
  Result:= FCount;

end;

function TGenericCircularQueue.GetIsEmpty: Boolean;
begin
  Result:= SoQ= EoQ;

end;

function TGenericCircularQueue.GetIsFull: Boolean;
begin
  Result:=(SoQ=((EoQ+ 1) mod Count));

end;

function TGenericCircularQueue.DoGetTop: T;
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  Result:= FData [SoQ];

end;

procedure TGenericCircularQueue.DoInsert(Entry: T);
begin
  if IsFull then
    raise EQueueIsFull.Create;


  FData [EoQ]:= Entry;
  EoQ:=(EoQ+ 1) mod Count;

end;

procedure TGenericCircularQueue.DoDelete(var LastElement: T);
begin
  if IsEmpty then
    raise EQueueIsEmpty.Create;

  LastElement:= FData[SoQ];
  SoQ:=(SoQ+ 1) mod Count;

end;

constructor TGenericCircularQueue.Create(Size: Integer);
begin                             
  inherited Create();

  SetLength(FData, Size);
  FCount:= Size;
  SoQ:= 0; EoQ:= 0;

end;

destructor TGenericCircularQueue.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    FData[i].Free;

  SetLength(FData, 0);

  inherited;

end;

procedure TGenericCircularQueue.Clear;
begin
  SetLength(FData, 0);
  FCount:= 0;

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

  LastElement:= T(FData[0]);
  FData.Delete(0);

end;

constructor TGenericQueue.Create;
begin
  inherited Create;

end;

destructor TGenericQueue.Destroy;
var
  i: Integer;

begin
  for i:= 0 to Count- 1 do
    T(FData.Items[i]).Free;


  inherited Destroy;

end;

procedure TGenericQueue.Clear;
begin
  FData.Clear;

end;

function TGenericQueue.GetCount: Integer;
begin
  Result:= FData.Count;

end;

function TGenericQueue.GetIsEmpty: Boolean;
begin
  Result:= Count= 0;

end;

function TGenericQueue.GetIsFull: Boolean;
begin
  Result:= False;

end;

function TGenericQueue.DoGetTop: T;
begin
  Result:= T(FData[0]);

end;

end.

