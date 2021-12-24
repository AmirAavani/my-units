unit PriorityQueueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, PairUnit;

type
  { TPQueue }

  generic TPQueue<TKey, TPriority>= class(TObject)
  private
    type
      TQElementSpec = specialize TPair<Tkey, TPriority>;

    TPairList = specialize TCollection<TQElementSpec>;
    var
      FMembers: TPairList;

    function GetCount: Integer; inline;
    function GetKey(Index: Integer): TKey;
    function GetMember(Index: Integer): TQElementSpec; inline;
    function GetPriority(Index: Integer): TPriority;
  public
   property Count : Integer read GetCount;
    property Key[Index: Integer]: TKey read GetKey;
    property Priority[Index: Integer]: TPriority read GetPriority;

    procedure Insert(K: TKey; Prior: TPriority);
    function Delete: TKey;

    constructor Create(Capacity: Integer = 100);
    destructor Destroy; override;
  protected
    property Member[Index: Integer] : TQElementSpec read GetMember;

  private
    function ParentIndex(Index: Integer): Integer; inline;
    procedure Heapify(Index: Integer);

  end;

implementation

{ TPQueue }

function TPQueue.GetMember(Index: Integer): TQElementSpec;
begin
  Result := FMembers[Index];

end;

function TPQueue.GetPriority(Index: Integer): TPriority;
begin
  Result := FMembers[Index].Second;

end;

function TPQueue.GetCount: Integer;
begin
  Result := FMembers.Count;

end;

function TPQueue.GetKey(Index: Integer): TKey;
begin
  Result := FMembers[Index].First;

end;

procedure TPQueue.Insert(K: TKey; Prior: TPriority);
var
  ActiveIndex: Integer;
  Temp: TQElementSpec;

begin
  FMembers.Add(specialize MakePair<Tkey, TPriority>(K, Prior));

  ActiveIndex := FMembers.Count - 1;
  if FMembers.Count = 1 then
    Exit;

  while Member[ParentIndex(ActiveIndex)].Second <
                       Member[ActiveIndex].Second do
  begin
    Temp := Member[ActiveIndex];
    FMembers[ActiveIndex]:= Member[ParentIndex(ActiveIndex)];
    FMembers[ParentIndex(ActiveIndex)]:= Temp;

    ActiveIndex := ParentIndex(ActiveIndex);
    if ActiveIndex = 0 then
      Break;

  end;

end;

function TPQueue.Delete: TKey;
begin
  Result := Member[0].First;

  FMembers.Items[0] := FMembers.Items[FMembers.Count - 1];
  FMembers.Count := Count - 1;

  Heapify(0);

end;

constructor TPQueue.Create(Capacity: Integer);
begin
  inherited Create;

  FMembers := TPairList.Create;
  FMembers.Capacity := Capacity;
end;

destructor TPQueue.Destroy;
begin
  FMembers.Free;

  inherited Destroy;
end;

function TPQueue.ParentIndex(Index: Integer): Integer;
begin
  Result := (Index - 1) shr 1;
end;

procedure TPQueue.Heapify (Index: Integer);
var
  ActiveIndex: Integer;
  MaxOfChildrenIndex: Integer;
  MaxOfChildren: TQElementSpec;
  Temp: TQElementSpec;

begin
  ActiveIndex:= Index;

  while 2 * ActiveIndex + 1 < Count do
  begin
    MaxOfChildrenIndex := 2 * ActiveIndex + 1;
    MaxOfChildren := Member[MaxOfChildrenIndex];

    if 2* ActiveIndex + 2 < Count then
      if Member[2 * ActiveIndex + 1].Second <
                         Member[2 * ActiveIndex + 2].Second then
      begin
        Inc(MaxOfChildrenIndex);
        MaxOfChildren := Member[MaxOfChildrenIndex];
      end;

    if MaxOfChildren.Second < Member[ActiveIndex].Second then
    begin
      Temp := Member[ActiveIndex];
      FMembers[ActiveIndex] := MaxOfChildren;
      FMembers[MaxOfChildrenIndex] := Temp;

      ActiveIndex := MaxOfChildrenIndex;
    end
    else
      Break;

  end;

end;


end.

