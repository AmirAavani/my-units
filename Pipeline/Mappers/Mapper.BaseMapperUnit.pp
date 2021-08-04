unit Mapper.BaseMapperUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Pipeline.DataLineUnit, Mapper.TypesUnit;

type
  TMapperFunction = function (kv: TKeyValue; Output: TDataLine): Boolean;

  { TBaseMapper }

  TBaseMapper = class(TObject)
  public
    class function MapperFromFunction(MapperFunc: TMapperFunction): TBaseMapper;

    function Map(kv: TKeyValue; Output: TDataLine): Boolean; virtual; abstract;
  end;

implementation

type

  { TFunctionBasedMapper }

  TFunctionBasedMapper = class(TBaseMapper)
  private
    FMapperFunction: TMapperFunction;

  public
    constructor Create(Func: TMapperFunction);

    function Map(kv: TKeyValue; Output: TDataLine): Boolean; override;
  end;

{ TFunctionBasedMapper }

constructor TFunctionBasedMapper.Create(Func: TMapperFunction);
begin
  inherited Create;

  FMapperFunction := Func;
end;

function TFunctionBasedMapper.Map(kv: TKeyValue; Output: TDataLine): Boolean;
begin
  Result := FMapperFunction(kv, Output);

end;

{ TBaseMapper }

class function TBaseMapper.MapperFromFunction(MapperFunc: TMapperFunction
  ): TBaseMapper;
begin
  Result := TFunctionBasedMapper.Create(MapperFunc);

end;

end.

