unit MapReduceUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GenericCollectionUnit, MapReduce.KeyValueUnit,
  MapReduce.MapperUnit, MapReduce.GraphUnit;

type
 { TMapper }

  TMapper = class(TObject)
  public
    constructor Create(const Input: TNode; Output: TNode);

    function Map: Boolean;

  end;

implementation

constructor TMapper.Create(const Input: TNode; Output: TNode);
begin

end;

function TMapper.Map: Boolean;
begin

end;

end.

