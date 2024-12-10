unit MapperUnits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.NodeUnit;

type

  { TCharCountMapper }

  TCharCountMapper = class(TMapper)
  public
    constructor Create;
  end;

implementation

{ TCharCountMapper }

constructor TCharCountMapper.Create;
begin
  inherited Create;

end;

end.

