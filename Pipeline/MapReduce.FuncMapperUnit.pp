unit MapReduce.FuncMapperUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.NodeUnit, ProtoHelperUnit;

type
  TMapperFunc = function (
  constref InputKey: AnsiString; InputValue: TBaseMessage;
  Sender: TNode): Boolean;

    { TFuncMapper }

  TFuncMapper = class(TMapper)
  protected
    MapperFn: TMapperFunc;

  public
    constructor Create(Fn: TMapperFunc);
  end;

  function CreateMapperFromFunc(
    MapperFunc: TMapperFunc;
    Sender: TObject): TFuncMapper;


implementation

function CreateMapperFromFunc(MapperFunc: TMapperFunc; Sender: TObject
  ): TFuncMapper;
begin
  Result := TFuncMapper.Create(MapperFunc);
end;


{ TFuncMapper }

constructor TFuncMapper.Create(Fn: TMapperFunc);
begin
  inherited Create;

  MapperFn := Fn;
end;



end.

