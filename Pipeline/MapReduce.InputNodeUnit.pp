unit MapReduce.InputNodeUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.NodeUnit, MapReduce.KeyValueUnit;

type
  TBaseInputNode = class(TNode)
  public
    constructor Create;

    function Next(var kv: TKeyValue): Boolean;

  end;

  { TInputNode }

  TInputNode = class(TNode)
  private
    FText: AnsiString;

  public
    constructor Create(constref t: AnsiString);

  end;

implementation

end.

