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

    function DeepCopy: _TObjectList;
  end;

  { TSimpleTypeList }

  generic TSimpleTypeList<TSimpleObject> = class(specialize TList<TSimpleObject>)
  private type
    TSimpleList = specialize TSimpleTypeList<TSimpleObject>;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    function DeepCopy: TSimpleList;
  end;

implementation

{ TSimpleTypeList }

function TSimpleTypeList.DeepCopy: TSimpleList;
var
  i: Integer;

begin
  if Self = nil then
    Exit(nil);

  Result := TSimpleList.Create;
  Result.Count := Self.Count;

  for i := 0 to Self.Count - 1 do
    Result[i] := Self[i];

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

function TObjectList.DeepCopy: _TObjectList;
var
  i: Integer;

begin
  if Self = nil then
    Exit(nil);

  Result := _TObjectList.Create;
  Result.Count := Self.Count;
  for i := 0 to Self.Count - 1 do
    Result[i] := Self[i];

end;

end.

