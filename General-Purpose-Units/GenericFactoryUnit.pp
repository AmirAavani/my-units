unit GenericFactoryUnit;
{assertions on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenericStackUnit, GenericCollectionUnit;

type
  { TGenericFactoy }

  generic TGenericFactoy<T>= class(TObject)
  private
    type
      TStackOfT= specialize TGenericStack<T>;
      TCollectionOfT= specialize TGenericCollection<T>;

    var
      AvailableItems: TStackOfT;
      AllItems: TCollectionOfT;

  public
    constructor Create(InitialMember: Integer = 0);
    destructor Destroy; override;

    function GetAllItems: TCollectionOfT;
    {
      Return an availabe member, if one exists, or create a new object of type T,
        otherwise.
    }
    function GetNewMember: T;
    {
    Returns the number of availabe members
    }
    function ReleaseMember(AMember: T): Integer;

  end;
implementation

{ TGenericFactoy }

constructor TGenericFactoy.Create(InitialMember: Integer);
var
  i: Integer;
  Obj: T;

begin
  inherited Create;

  AvailableItems:= TStackOfT.Create;
  AllItems:= TCollectionOfT.Create;
//  AllItems.Capacity:= InitialMember+ 1;

  for i:= 1 to InitialMember do
  begin
    Obj:= T.Create;
    AvailableItems.Push(Obj);
    AllItems.PushBack(Obj);

  end;

end;

destructor TGenericFactoy.Destroy;
var
  i: Integer;

begin
  AllItems.Clear;
  AllItems.Free;
  AvailableItems.Clear;
  AvailableItems.Free;

  inherited Destroy;
end;

function TGenericFactoy.GetAllItems: TCollectionOfT;
begin
  Result := AllItems;
end;

function TGenericFactoy.GetNewMember: T;
begin
  if not AvailableItems.IsEmpty then
  begin
    Result := AvailableItems.Pop;
  end
  else
  begin
    Result := T.Create;
    AllItems.PushBack(Result);
  end;

end;

function TGenericFactoy.ReleaseMember(AMember: T): Integer;
begin
  if AMember <> nil then
  begin
    AMember.Reset;
    assert(not AvailableItems.Find(AMember));
    AvailableItems.Push(AMember);
  end;

end;

end.

