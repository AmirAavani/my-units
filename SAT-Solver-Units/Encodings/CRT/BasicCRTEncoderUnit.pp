unit BasicCRTEncoderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CRTEncoderUnit, CRTConstraintUnit, BaseEncoderUnit, BitVectorUnit;

type

  { TBasicCRTEncoder }

  TBasicCRTEncoder = class(TBaseCRTEncoder)
  private
  protected
    function EncodeConstraint(Constraint: TCRTConstraint): TEncoding; override;
  public

  end;


implementation
uses
  ClauseUnit, TSeitinVariableUnit, SatSolverInterfaceUnit, NumberTheoryUnit;

{ TBasicCRTEncoder }

function TBasicCRTEncoder.EncodeConstraint(Constraint: TCRTConstraint
  ): TEncoding;
var
  cl: TClauseCollection;
  Lit: TLiteral;

begin
  SatSolverInterfaceUnit.ReNewSatSolver('CNFCollection');
{


}

  Lit := CreateLiteral(GetVariableManager.CreateNewVariable, False);

  Result := TEncoding.Create(GetSatSolver.CNF.Copy, Lit);
  SatSolverInterfaceUnit.PopBackSatSolver;


end;

initialization
  TBaseEncoder.RegisterEncoder(TBasicCRTEncoder);

end.

