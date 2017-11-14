unit BaseConstraintUnit;
interface


{$mode objfpc}{$H+}

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

