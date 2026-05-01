unit ProtoHelperListsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  { TObjectList }

  generic TObjectList<TMyObject> = class(specialize TList<TMyObject>)
  private type
    _TObjectList = specialize TObjectList<TMyObject>;
  protected
    function GetCount: SizeInt; override;

  public
    property Count: SizeInt read GetCount;  // Redeclare to use overridden getter
    
    destructor Destroy; override;

    function ItemsPtr: Pointer; inline;
  end;

  { TSimpleTypeList }

  generic TSimpleTypeList<TSimpleType> = class(specialize TList<TSimpleType>)
  private type
    TSimpleList = specialize TSimpleTypeList<TSimpleType>;
  protected
    function GetCount: SizeInt; override;

  public
    property Count: SizeInt read GetCount;  // Redeclare to use overridden getter
    
    constructor Create;
    destructor Destroy; override;

    function DeepCopy: TSimpleList;
    function ItemsPtr: Pointer; inline;

  end;

implementation

{ TSimpleTypeList }

function TSimpleTypeList.DeepCopy: TSimpleList;
var
  NewCount: SizeInt;
begin
  if Self = nil then
    Exit(nil);

  Result := TSimpleList.Create;
  NewCount := Self.Count;
  Result.SetCapacity(NewCount);  // Pre-allocate
  System.Move(
    Self.ItemsPtr^,
    Result.ItemsPtr^,
    NewCount * SizeOf(TSimpleType)
  );

end;

function TSimpleTypeList.ItemsPtr: Pointer;
begin
  if Self = nil then
    Exit(nil);

  Result := @Self.FItems[0];
end;

function TSimpleTypeList.GetCount: SizeInt;
begin
  if Self = nil then
  begin
    WriteLn('I am doing my job!');
    Flush(Output);
    Exit(0);
  end;

  Result := inherited GetCount;
end;


constructor TSimpleTypeList.Create;
begin
  inherited Create;

end;

destructor TSimpleTypeList.Destroy;
begin

  inherited Destroy;
end;

{ TObjectList }

function TObjectList.GetCount: SizeInt;
begin
  if Self = nil then
    Exit(0);

  Result := inherited GetCount;
end;

destructor TObjectList.Destroy;
var
  Obj: TObject;

begin
  for Obj in Self do
    Obj.Free;

  inherited Destroy;

end;

function TObjectList.ItemsPtr: Pointer;
begin
  Result := @Self.FItems[0];

end;

end.

