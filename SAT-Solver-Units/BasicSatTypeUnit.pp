unit BasicSatTypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TVariable = Integer;
  TLiteral = Integer;
  TIntList = specialize TFPGList<TLiteral>;
  TClause = specialize TFPGList<TLiteral>;
  TClauses = specialize TFPGList<TClause>;


implementation

end.

