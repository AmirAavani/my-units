unit ProtoHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ProtoHelperListsUnit, ProtoStreamUnit;

type
  { TBaseProtoObject }

  TBaseProtoObject = class(TObject)
  private
    function GetMember(Index: Integer): TBaseProtoObject;
    procedure SetMember(Index: Integer; AValue: TBaseProtoObject);
  protected
    FMembers: specialize TObjectList<TBaseProtoObject>;

  public
    property Member[Index: Integer]: TBaseProtoObject read GetMember write SetMember;

    constructor Create(MemberCount: Integer);
    destructor Destroy; override;

    function LoadFromStream(Stream: TStream): Boolean; virtual; abstract;
    procedure SaveToStream(Stream: TStream);  virtual; abstract;

  end;

implementation
{ TBaseProtoObject }

function TBaseProtoObject.GetMember(Index: Integer): TBaseProtoObject;
begin
  Result := FMembers[Index];

end;

procedure TBaseProtoObject.SetMember(Index: Integer; AValue: TBaseProtoObject);
begin
  FMembers[Index] := AValue;

end;

constructor TBaseProtoObject.Create(MemberCount: Integer);
begin
  inherited Create;

  FMembers := specialize TObjectList<TBaseProtoObject>.Create;
  FMembers.Count := MemberCount;

end;

destructor TBaseProtoObject.Destroy;
begin
  FMembers.Free;

  inherited Destroy;
end;

end.

