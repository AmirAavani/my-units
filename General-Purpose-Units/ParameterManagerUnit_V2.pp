unit ParameterManagerUnit_V2;

{$mode ObjFPC}{$H+}

interface

uses
  ValueUnit, Classes, SysUtils, ParameterManagerUnit;

type

{$M+}
  { TBaseParam }

  TBaseParam = class(TObject)
  private
    FDebug: Integer;

  public
    property Debug: Integer read FDebug;

  public
    constructor Create(Values: TRunTimeParameterManager.TNameValueMap);

  end;

  { TRunTimeParameterManager }
  generic TRunTimeParameterManager<TParamList> =
    class(ParameterManagerUnit.TRunTimeParameterManager)
  protected
    FParams: TParamList;

  public
    property Params: TParamList read FParams;
    constructor Create;

  end;

implementation

uses
  TypInfo;

{ TBaseParam }

constructor TBaseParam.Create(Values: TRunTimeParameterManager.TNameValueMap);
var
  Value: ValueUnit.TValue;

begin
  inherited Create;

  if Values.TryGetData('--Debug', Value) then
    Self.FDebug := Value.AsInteger;

end;

{ TRunTimeParameterManager }

constructor TRunTimeParameterManager.Create;
begin
  inherited Create;

  FParams := TParamList.Create(Self.Values);

end;

end.

