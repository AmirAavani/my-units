unit BaseConstraintUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TBaseConstraint }

  TBaseConstraint = class(TObject)
  public
    function ToString: AnsiString; virtual; abstract;

  end;

implementation

end.

