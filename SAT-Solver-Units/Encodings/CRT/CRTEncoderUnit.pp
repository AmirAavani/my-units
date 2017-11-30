unit CRTEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseEncoderUnit, BaseConstraintUnit, CRTConstraintUnit,
  BitVectorUnit;

type

  { TBaseCRTEncoder }

  TBaseCRTEncoder = class(TBaseEncoder)
  protected
    function EncodeConstraint(Constraint: TCRTConstraint): TEncoding; virtual; abstract;

  public
    function Encode(Problem: TBaseConstraint): TEncoding; override;

  end;


implementation
uses
  BasicCRTEncoderUnit;

{ TBaseCRTEncoder }

function TBaseCRTEncoder.Encode(Problem: TBaseConstraint): TEncoding;
begin
  Result := Self.EncodeConstraint(Problem as TCRTConstraint);
end;

initialization

end.

