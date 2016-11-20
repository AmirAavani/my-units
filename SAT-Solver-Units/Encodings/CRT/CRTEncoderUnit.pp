unit CRTEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseEncoderUnit, CRTProblemUnit, BaseProblemUnit;

type

  { TBaseCRTEncoder }

  TBaseCRTEncoder = class(TBaseEncoder)
  protected
    function Encode(Problem: TCRTProblem): TEncoding; virtual; abstract;
  public
    function Encode(Problem: TBaseProblem): TEncoding; override;
    function GetEncoder(const EncoderName: AnsiString): TBaseEncoder; override;

  end;


implementation

{ TBaseCRTEncoder }

function TBaseCRTEncoder.Encode(Problem: TBaseProblem): TEncoding;
begin
  Result := Self.Encode(Problem as TCRTProblem);
end;

function TBaseCRTEncoder.GetEncoder(const EncoderName: AnsiString
  ): TBaseEncoder;
begin
//  if EncoderName =
  Result := nil;
end;


end.

