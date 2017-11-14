unit CRTEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseEncoderUnit, BaseConstraintUnit, CRTUnit;

type

  { TBaseCRTEncoder }

  TBaseCRTEncoder = class(TBaseEncoder)
  protected
    function EncodeConstraint(Constraint: TCRTConstraint): TEncoding; virtual; abstract;

  public
    function Encode(Problem: TBaseConstraint): TEncoding; override;
    class function GetEncoder(const EncoderName: AnsiString): TBaseEncoder; override;

  end;


implementation
uses
  BasicCRTEncoderUnit;

{ TBaseCRTEncoder }

function TBaseCRTEncoder.Encode(Problem: TBaseConstraint): TEncoding;
begin
  Result := Self.EncodeConstraint(Problem as TCRTConstraint);
end;

class function TBaseCRTEncoder.GetEncoder(const EncoderName: AnsiString
  ): TBaseEncoder;
begin
  Result := nil;
  if EncoderName = 'BasicEncoder' then
    Result := TBasicCRTEncoder.Create;
end;


end.

