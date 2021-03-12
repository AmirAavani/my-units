unit NamedObjectListUnit;

{$mode objfpc}{$H+}

interface

uses
   Generics.Collections, Classes, SysUtils;

type
  { TNamedObjectList }

  generic TNamedObjectList<TObject> = class(specialize TList<TObject>)
  private
    function GetByName(aName: AnsiString): TObject;
  public
    property ByName[aName: AnsiString]: TObject read GetByName;
    destructor Destroy; override;

    function ToXML: AnsiString;
  end;



implementation

{ TNamedObjectList }

function TNamedObjectList.GetByName(aName: AnsiString): TObject;
var
  anObj: TObject;

begin
  for anObj in Self do
    if anObj.Name = aName then
      Exit(anObj);

  Exit(nil);
end;

destructor TNamedObjectList.Destroy;
var
  Obj: TObject;

begin
  for Obj in Self do
    Obj.Free;

  inherited Destroy;

end;

function TNamedObjectList.ToXML: AnsiString;
var
  Obj: TObject;
  NodeName: AnsiString;

begin
  if Self = nil then
    Exit('');
  if Self.Count = 0 then
    Exit('');

  // ClassName is in form of TNamedObjectList<PBDefinitionUnit.ClassName>
  NodeName := Copy(ClassName, Pos('.', ClassName) + 1, Length(ClassName) - Pos('.', ClassName) - 1) + 's';

  Result := Format('<%s>'#10, [NodeName]);

  for Obj in Self do
    Result += Obj.ToXML;

  Result += Format('</%s>'#10, [NodeName]);
end;

end.

