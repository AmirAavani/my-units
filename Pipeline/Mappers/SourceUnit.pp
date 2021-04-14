unit SourceUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSource }

  TBaseSource = class(TObject)
  public
    constructor Create;

  end;

implementation

{ TSource }

constructor TBaseSource.Create;
begin
  inherited Create;

end;

end.

