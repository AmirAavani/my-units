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

  public
    destructor Destroy; override;

    function ItemsPtr: Pointer; inline;
  end;

  { TSimpleTypeList }

  generic TSimpleTypeList<TSimpleType> = class(specialize TList<TSimpleType>)
  private type
    TSimpleList = specialize TSimpleTypeList<TSimpleType>;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    function DeepCopy: TSimpleList;
    function ItemsPtr: Pointer; inline;

  end;

implementation

{ TSimpleTypeList }

function TSimpleTypeList.DeepCopy: TSimpleList;
begin
  if Self = nil then
    Exit(nil);

  Result := TSimpleList.Create;
  Result.Count := Self.Count;
  System.Move(
    Self.ItemsPtr^,
    Result.ItemsPtr^,
    Self.Count * SizeOf(TSimpleType)
  );

end;

function TSimpleTypeList.ItemsPtr: Pointer;
begin
  Result := @Self.FItems[0];
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

