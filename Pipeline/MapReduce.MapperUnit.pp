unit MapReduce.MapperUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.KeyValueUnit;

type
  TSource = interface
    function Next(var kv: TKeyValue): Boolean; virtual; abstract;
  end;

  { TSink }

  TSink = interface
    procedure Put(kv: TKeyValue);

  end;

  //
  TFuncMapper = function (constref kv: TKeyValue; Sink: TSink): Boolean;

  { TMapper }

  TMapper = class(TObject)
  public
    constructor Create;
    constructor CreateFromFuncMapper(
      FuncMapper: TFuncMapper);
    destructor Destroy; override;

    // Run(Source: TSource; Sink: TSink);

  end;


implementation

{ TMapper }

constructor TMapper.Create;
begin

end;

constructor TMapper.CreateFromFuncMapper(FuncMapper: TFuncMapper);
begin

end;

destructor TMapper.Destroy;
begin
  inherited Destroy;
end;

end.

