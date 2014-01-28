unit FactoringUsingSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BigInt, GenericCollectionUnit;

type
  TBigIntCollection= specialize TGenericCollection<TBigInt>;

  { TBaseFactorizerUsingSAT }

  TBaseFactorizerUsingSAT= class (TObject)
  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateCNF (n: TBigInt); virtual; abstract;

  end;

  function GetActiveFactorizer: TBaseFactorizerUsingSAT;

  procedure Initialize;// (FactorizerMode: AnsiString);
  procedure Finalize;

implementation
uses
  BinaryEncodingForFactoringUnit, ParameterManagerUnit,
  FactoringUsingModulos;

{ TBaseFactorizerUsingSAT }

constructor TBaseFactorizerUsingSAT.Create;
begin
  inherited Create;


end;

destructor TBaseFactorizerUsingSAT.Destroy;
begin

end;

var
  ActiveFactorizer: TBaseFactorizerUsingSAT;

function GetActiveFactorizer: TBaseFactorizerUsingSAT;
begin
  Result:= ActiveFactorizer;

end;

procedure Initialize;// (FactorizerMode: AnsiString);
begin
  WriteLn ('Modulo.Prime.Unary');
  if UpperCase (GetRunTimeParameterManager.GetValueByName ('--FactorizerMode'))=
           UpperCase ('BinaryRep') then
    ActiveFactorizer:= TBinaryRepBasedFactorizer.Create
  else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--FactorizerMode'))=
          UpperCase ('Modulo.Prime.Unary') then
    ActiveFactorizer:= TPrimesUnaryFactorizer.Create
  else if UpperCase (GetRunTimeParameterManager.GetValueByName ('--FactorizerMode'))=
           UpperCase ('Modulo.Prime.Binary') then
    ActiveFactorizer:= TPrimesBinaryFactorizer.Create
  else
    raise Exception.Create ('Invalid Factorizer Mode!');

end;

procedure Finalize;
begin
  ActiveFactorizer.Free;
  ActiveFactorizer:= nil;

end;

initialization
  ActiveFactorizer:= nil;

finalization
  Finalize;

end.
