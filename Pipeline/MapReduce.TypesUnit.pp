unit MapReduce.TypesUnit;

{$mode ObjFPC}{$H+}

interface
uses
  sysutils;

type
   { EMapReduce }

  EMapReduce = class(Exception)
  public
    constructor Create(constref Msg: AnsiString);
  end;

implementation

{ EMapReduce }

constructor EMapReduce.Create(constref Msg: AnsiString);
begin
  inherited Create(Msg);

end;

end.

