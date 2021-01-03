unit ProtoHelperListsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  { TObjectList }

  generic TObjectList<TMyObject> = class(specialize TList<TMyObject>)
  public
    destructor Destroy; override;

    function ToString: AnsiString; override;
    function LoadFromStream(Stream: TStream): Boolean;
    procedure SaveToStream(Stream: TStream);
  end;

  { TSimpleTypeList }

  generic TSimpleTypeList<TSimpleObject> = class(specialize TList<TSimpleObject>)
  protected
    function SimpleObjectToString(Obj: TSimpleObject): AnsiString; virtual; abstract;

  public

    constructor Create;
    destructor Destroy; override;

    function ToString: AnsiString; override;
    function LoadFromStream(Stream: TStream): Boolean;
    procedure SaveToStream(Stream: TStream);
  end;

  { TBooleanList }

  TBooleanList = class(specialize TSimpleTypeList<Boolean>)
  public
    function ToString: AnsiString; override;
  end;

implementation

{ TBooleanList }

function TBooleanList.ToString: AnsiString;
var
  Data: Boolean;

begin
  Result := '[';
  for data in Self do
  begin
    if Length(Result) <> 1 then
      Result += ', ';
    Result += BoolToStr(data)
  end;
  Result += ']';

end;

{ TSimpleTypeList }

constructor TSimpleTypeList.Create;
begin
  inherited Create;

end;

destructor TSimpleTypeList.Destroy;
begin
  inherited Destroy;
end;

function TSimpleTypeList.ToString: AnsiString;
var
  Data: TSimpleObject;

begin
  Result := '[';
  for data in Self do
  begin
    if Length(Result) <> 1 then
      Result += ', ';
    Result += SimpleObjectToString(data);
  end;
  Result += ']';

end;

function TSimpleTypeList.LoadFromStream(Stream: TStream): Boolean;
begin
  raise Exception.Create('NIY');
end;

procedure TSimpleTypeList.SaveToStream(Stream: TStream);
begin
  raise Exception.Create('NIY');
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

function TObjectList.ToString: AnsiString;
var
  Obj: TObject;

begin
  Result := '';
  for Obj in Self do
    Result += Obj.ToString;
end;

function TObjectList.LoadFromStream(Stream: TStream): Boolean;
begin
  Halt(1);
end;

procedure TObjectList.SaveToStream(Stream: TStream);
begin
  Halt(2);
end;

end.

