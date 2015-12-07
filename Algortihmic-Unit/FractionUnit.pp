unit FractionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TFraction }

  TFraction = class(TObject)
  private
    FDenominator: uInt64;
    FNumerator: uInt64;
  public
    property Numerator: uInt64 read FNumerator;
    property Denominator: uInt64 read FDenominator;

    constructor Create;
    constructor Create(a, b: uInt64);

    { Adds Self and AnotherFrac and returns Self}
    function Add(AnotherFrac: TFraction): TFraction;
    { Updates Self to be 1/Self and returns Self}
    function Reverse: TFraction;
    function ToString: AnsiString;

    protected
      procedure Simplify;
  end;

implementation

{ TFraction }

constructor TFraction.Create;
begin
  inherited Create;

  FNumerator := 0;
  FDenominator := 1;
end;

constructor TFraction.Create(a, b: uInt64);
begin
  inherited Create;

  FNumerator := a;
  FDenominator := b;

end;

function gcd(a, b: uInt64): uInt64;
begin
  if a < b then
    Exit(gcd(b, a));

  Result := b;
  while a mod b <> 0 do
  begin
    Result := a mod b;
    a := b;
    b := Result;
  end;
end;

function TFraction.Add(AnotherFrac: TFraction): TFraction;
var
  a, b, c: uInt64;
begin
  c := gcd(Self.Denominator, AnotherFrac.Denominator);
  b := (Self.Denominator div c) * AnotherFrac.Denominator;
  a := Self.Numerator * (AnotherFrac.Denominator div c) +
       AnotherFrac.Numerator * (Self.Denominator div c);

  Self.FNumerator := a;
  Self.FDenominator := b;
  Simplify;

  Result := Self;
end;

function TFraction.Reverse: TFraction;
begin
  Self.FDenominator := Self.Numerator xor Self.Denominator;
  Self.FNumerator := Self.Numerator xor Self.Denominator;
  Self.FDenominator := Self.Numerator xor Self.Denominator;

  Result := Self;
end;

function TFraction.ToString: AnsiString;
begin
  Result := IntToStr(Numerator) + '/' + IntToStr(Denominator);
end;

procedure TFraction.Simplify;
var
  c: uInt64;

begin
  c := gcd(FNumerator, FDenominator);
  FNumerator := FNumerator div c;
  FDenominator := FDenominator div c;
end;

end.

